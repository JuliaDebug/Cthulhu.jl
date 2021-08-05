using Core.Compiler: AbstractInterpreter, NativeInterpreter, InferenceState,
    OptimizationState, CodeInfo, CodeInstance, InferenceResult, WorldRange,
    IRCode, SSAValue, inlining_policy

struct InferredSource
    src::CodeInfo
    stmt_infos::Vector{Any}
    rt::Any
end

struct OptimizedSource
    ir::IRCode
    isinlineable::Bool
end

mutable struct CthulhuInterpreter <: AbstractInterpreter
    native::NativeInterpreter

    unopt::Dict{Union{MethodInstance, InferenceResult}, InferredSource}
    opt::Dict{MethodInstance, CodeInstance}

    msgs::Dict{MethodInstance, Vector{Pair{Int, String}}}
end

CthulhuInterpreter() = CthulhuInterpreter(
    NativeInterpreter(),
    Dict{MethodInstance, InferredSource}(),
    Dict{MethodInstance, CodeInstance}(),
    Dict{MethodInstance, Vector{Pair{Int, String}}}()
)

import Core.Compiler: InferenceParams, OptimizationParams, get_world_counter,
    get_inference_cache, code_cache,
    WorldView, lock_mi_inference, unlock_mi_inference, InferenceState
using Base: @invoke

Compiler.InferenceParams(interp::CthulhuInterpreter) = InferenceParams(interp.native)
Compiler.OptimizationParams(interp::CthulhuInterpreter) = OptimizationParams(interp.native)
Compiler.get_world_counter(interp::CthulhuInterpreter) = get_world_counter(interp.native)
Compiler.get_inference_cache(interp::CthulhuInterpreter) = get_inference_cache(interp.native)

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
    push!(get!(interp.msgs, sv.linfo, Tuple{Int, String}[]),
        sv.currpc => msg)
end

function Compiler.finish(state::InferenceState, interp::CthulhuInterpreter)
    r = @invoke Compiler.finish(state::InferenceState, interp::AbstractInterpreter)
    interp.unopt[Core.Compiler.any(state.result.overridden_by_const) ? state.result : state.linfo] = InferredSource(
        copy(isa(state.src, OptimizationState) ?
            state.src.src : state.src),
        copy(state.stmt_info),
        state.result.result)
    return r
end

function Compiler.transform_result_for_cache(interp::CthulhuInterpreter, linfo::MethodInstance,
        valid_worlds::Core.Compiler.WorldRange, @nospecialize(inferred_result))
    if isa(inferred_result, OptimizationState)
        opt = inferred_result
        if isdefined(opt, :ir)
            return OptimizedSource(opt.ir::IRCode, opt.src.inlineable)
        end
    end
    return inferred_result
end

function Compiler.inlining_policy(interp::CthulhuInterpreter)
    function (src)
        @assert isa(src, OptimizedSource)
        return src.isinlineable ? src.ir : nothing
    end
end

function Compiler.finish!(interp::CthulhuInterpreter, caller::InferenceResult)
    if isa(caller.src, OptimizationState)
        opt = caller.src
        if isdefined(opt, :ir)
            caller.src = OptimizedSource(opt.ir, opt.src.inlineable)
        end
    end
end
