using Core.Compiler: AbstractInterpreter, NativeInterpreter, InferenceState,
    OptimizationState, CodeInfo, CodeInstance, InferenceResult, WorldRange,
    IRCode, SSAValue, inlining_policy

struct InferredSource
    src::CodeInfo
    stmt_info::Vector{Any}
    effects::Effects
    rt::Any
    InferredSource(src::CodeInfo, stmt_info::Vector{Any}, effects, @nospecialize(rt)) =
        new(src, stmt_info, effects, rt)
end

struct OptimizedSource
    ir::IRCode
    src::CodeInfo
    isinlineable::Bool
end

const Remarks = Vector{Pair{Int, String}}

struct CthulhuInterpreter <: AbstractInterpreter
    native::AbstractInterpreter

    unopt::Dict{Union{MethodInstance, InferenceResult}, InferredSource}
    opt::Dict{MethodInstance, CodeInstance}

    remarks::Dict{MethodInstance, Remarks}
end

CthulhuInterpreter(interp::AbstractInterpreter=NativeInterpreter()) =
    CthulhuInterpreter(
        interp,
        Dict{MethodInstance, InferredSource}(),
        Dict{MethodInstance, CodeInstance}(),
        Dict{MethodInstance, Remarks}()
    )

import Core.Compiler: InferenceParams, OptimizationParams, get_world_counter,
    get_inference_cache, code_cache, method_table,
    WorldView, lock_mi_inference, unlock_mi_inference, InferenceState
using Base: @invoke

Compiler.InferenceParams(interp::CthulhuInterpreter) = InferenceParams(interp.native)
Compiler.OptimizationParams(interp::CthulhuInterpreter) = OptimizationParams(interp.native)
Compiler.get_world_counter(interp::CthulhuInterpreter) = get_world_counter(interp.native)
Compiler.get_inference_cache(interp::CthulhuInterpreter) = get_inference_cache(interp.native)
Compiler.method_table(interp::CthulhuInterpreter, sv::InferenceState) = method_table(interp.native, sv)

# No need to do any locking since we're not putting our results into the runtime cache
Compiler.lock_mi_inference(interp::CthulhuInterpreter, mi::MethodInstance) = nothing
Compiler.unlock_mi_inference(interp::CthulhuInterpreter, mi::MethodInstance) = nothing

struct CthulhuCache
    cache::Dict{MethodInstance, CodeInstance}
end

Compiler.code_cache(interp::CthulhuInterpreter) = WorldView(CthulhuCache(interp.opt), WorldRange(get_world_counter(interp)))
Compiler.get(wvc::WorldView{CthulhuCache}, mi::MethodInstance, default) = get(wvc.cache.cache, mi, default)
Compiler.haskey(wvc::WorldView{CthulhuCache}, mi::MethodInstance) = haskey(wvc.cache.cache, mi)
Compiler.setindex!(wvc::WorldView{CthulhuCache}, ci::CodeInstance, mi::MethodInstance) = setindex!(wvc.cache.cache, ci, mi)

Compiler.may_optimize(interp::CthulhuInterpreter) = true
Compiler.may_compress(interp::CthulhuInterpreter) = false
Compiler.may_discard_trees(interp::CthulhuInterpreter) = false
Compiler.verbose_stmt_info(interp::CthulhuInterpreter) = true

function Compiler.add_remark!(interp::CthulhuInterpreter, sv::InferenceState, msg)
    if !haskey(interp.remarks, sv.linfo)
        interp.remarks[sv.linfo] = Remarks()
    end
    push!(interp.remarks[sv.linfo], sv.currpc => msg)
end

function Compiler.finish(state::InferenceState, interp::CthulhuInterpreter)
    r = @invoke Compiler.finish(state::InferenceState, interp::AbstractInterpreter)
    interp.unopt[Core.Compiler.any(state.result.overridden_by_const) ? state.result : state.linfo] = InferredSource(
        copy(state.src),
        copy(state.stmt_info),
        isdefined(Core.Compiler, :Effects) ? state.ipo_effects : nothing,
        state.result.result)
    return r
end

function Compiler.transform_result_for_cache(interp::CthulhuInterpreter, linfo::MethodInstance,
        valid_worlds::WorldRange, @nospecialize(inferred_result))
    if isa(inferred_result, OptimizationState)
        opt = inferred_result
        ir = opt.ir
        if ir !== nothing
            return OptimizedSource(ir, opt.src, opt.src.inlineable)
        end
    end
    return inferred_result
end

# branch on https://github.com/JuliaLang/julia/pull/41328
@static if isdefined(Compiler, :is_stmt_inline)
function Compiler.inlining_policy(
    interp::CthulhuInterpreter, @nospecialize(src), stmt_flag::UInt8,
    mi::MethodInstance, argtypes::Vector{Any})
    @assert isa(src, OptimizedSource) || isnothing(src)
    if isa(src, OptimizedSource)
        if Compiler.is_stmt_inline(stmt_flag) || src.isinlineable
            return src.ir
        end
    else
        # the default inlining policy may try additional effor to find the source in a local cache
        return @invoke Compiler.inlining_policy(
            interp::AbstractInterpreter, nothing, stmt_flag::UInt8,
            mi::MethodInstance, argtypes::Vector{Any})
    end
    return nothing
end
else # @static if isdefined(Compiler, :is_stmt_inline)
function Compiler.inlining_policy(interp::CthulhuInterpreter)
    function (src)
        @assert isa(src, OptimizedSource)
        return src.isinlineable ? src.ir : nothing
    end
end
end # @static if isdefined(Compiler, :is_stmt_inline)

function Compiler.finish!(interp::CthulhuInterpreter, caller::InferenceResult)
    src = caller.src
    if isa(src, OptimizationState)
        opt = src
        ir = opt.ir
        if ir !== nothing
            caller.src = OptimizedSource(ir, opt.src, opt.src.inlineable)
        end
    end
end
