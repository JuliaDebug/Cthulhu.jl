using .CC: AbstractInterpreter, NativeInterpreter, InferenceState, OptimizationState,
    CodeInfo, CodeInstance, InferenceResult, WorldRange, WorldView, IRCode, SSAValue

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
    effects::Effects
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

import .CC: InferenceParams, OptimizationParams, get_world_counter,
    get_inference_cache, code_cache, lock_mi_inference, unlock_mi_inference, method_table,
    inlining_policy
using Base: @invoke

CC.InferenceParams(interp::CthulhuInterpreter) = InferenceParams(interp.native)
CC.OptimizationParams(interp::CthulhuInterpreter) = OptimizationParams(interp.native)
CC.get_world_counter(interp::CthulhuInterpreter) = get_world_counter(interp.native)
CC.get_inference_cache(interp::CthulhuInterpreter) = get_inference_cache(interp.native)

# No need to do any locking since we're not putting our results into the runtime cache
CC.lock_mi_inference(interp::CthulhuInterpreter, mi::MethodInstance) = nothing
CC.unlock_mi_inference(interp::CthulhuInterpreter, mi::MethodInstance) = nothing
@static if v"1.8-beta2" <= VERSION < v"1.9-" || VERSION >= v"1.9.0-DEV.120"
CC.method_table(interp::CthulhuInterpreter) = method_table(interp.native)
else # if v"1.8-beta2" <= VERSION < v"1.9-" || VERSION >= v"1.9.0-DEV.120"
CC.method_table(interp::CthulhuInterpreter, sv::InferenceState) = method_table(interp.native, sv)
end # if v"1.8-beta2" <= VERSION < v"1.9-" || VERSION >= v"1.9.0-DEV.120"
struct CthulhuCache
    cache::Dict{MethodInstance, CodeInstance}
end

CC.code_cache(interp::CthulhuInterpreter) = WorldView(CthulhuCache(interp.opt), WorldRange(get_world_counter(interp)))
CC.get(wvc::WorldView{CthulhuCache}, mi::MethodInstance, default) = get(wvc.cache.cache, mi, default)
CC.haskey(wvc::WorldView{CthulhuCache}, mi::MethodInstance) = haskey(wvc.cache.cache, mi)
CC.setindex!(wvc::WorldView{CthulhuCache}, ci::CodeInstance, mi::MethodInstance) = setindex!(wvc.cache.cache, ci, mi)

CC.may_optimize(interp::CthulhuInterpreter) = true
CC.may_compress(interp::CthulhuInterpreter) = false
CC.may_discard_trees(interp::CthulhuInterpreter) = false
CC.verbose_stmt_info(interp::CthulhuInterpreter) = true

function CC.add_remark!(interp::CthulhuInterpreter, sv::InferenceState, msg)
    if !haskey(interp.remarks, sv.linfo)
        interp.remarks[sv.linfo] = Remarks()
    end
    push!(interp.remarks[sv.linfo], sv.currpc => msg)
end

function CC.finish(state::InferenceState, interp::CthulhuInterpreter)
    res = @invoke CC.finish(state::InferenceState, interp::AbstractInterpreter)
    key = CC.any(state.result.overridden_by_const) ? state.result : state.linfo
    interp.unopt[key] = InferredSource(
        copy(state.src),
        copy(state.stmt_info),
        isdefined(CC, :Effects) ? state.ipo_effects : nothing,
        state.result.result)
    return res
end

function create_cthulhu_source(@nospecialize(x), effects::Effects)
    isa(x, OptimizationState) || return x
    ir = x.ir::IRCode
    return OptimizedSource(ir, x.src, x.src.inlineable, effects)
end

@static if hasmethod(CC.transform_result_for_cache,
    (AbstractInterpreter, MethodInstance, WorldRange, Any, Effects))
    function CC.transform_result_for_cache(interp::CthulhuInterpreter,
        linfo::MethodInstance, valid_worlds::WorldRange, @nospecialize(inferred_result),
        ipo_effects::CC.Effects)
        return create_cthulhu_source(inferred_result, ipo_effects)
    end
else
    function CC.transform_result_for_cache(interp::CthulhuInterpreter,
        linfo::MethodInstance, valid_worlds::WorldRange, @nospecialize(inferred_result))
        return create_cthulhu_source(inferred_result, Effects())
    end
end

# branch on https://github.com/JuliaLang/julia/pull/41328
@static if isdefined(CC, :is_stmt_inline)
function CC.inlining_policy(
    interp::CthulhuInterpreter, @nospecialize(src), stmt_flag::UInt8,
    mi::MethodInstance, argtypes::Vector{Any})
    @assert isa(src, OptimizedSource) || isnothing(src)
    if isa(src, OptimizedSource)
        if CC.is_stmt_inline(stmt_flag) || src.isinlineable
            return src.ir
        end
    else
        # the default inlining policy may try additional effor to find the source in a local cache
        return @invoke CC.inlining_policy(
            interp::AbstractInterpreter, nothing, stmt_flag::UInt8,
            mi::MethodInstance, argtypes::Vector{Any})
    end
    return nothing
end
else # @static if isdefined(CC, :is_stmt_inline)
function CC.inlining_policy(interp::CthulhuInterpreter)
    function (src)
        @assert isa(src, OptimizedSource)
        return src.isinlineable ? src.ir : nothing
    end
end
end # @static if isdefined(CC, :is_stmt_inline)

function CC.finish!(interp::CthulhuInterpreter, caller::InferenceResult)
    effects = EFFECTS_ENABLED ? caller.ipo_effects : nothing
    caller.src = create_cthulhu_source(caller.src, effects)
end
