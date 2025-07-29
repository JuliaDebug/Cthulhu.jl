struct InferredSource
    src::CodeInfo
    stmt_info::Vector{CCCallInfo}
    effects::Effects
    rt::Any
    exct::Any
    InferredSource(src::CodeInfo, stmt_info::Vector{CCCallInfo}, effects, @nospecialize(rt),
                   @nospecialize(exct)) =
        new(src, stmt_info, effects, rt, exct)
end

struct OptimizedSource
    ir::IRCode
    src::CodeInfo
    isinlineable::Bool
    effects::Effects
end

const InferenceKey = Union{CodeInstance,InferenceResult} # TODO make this `CodeInstance` fully
const InferenceDict{InferenceValue} = IdDict{InferenceKey, InferenceValue}
const PC2Remarks = Vector{Pair{Int, String}}
const PC2CallMeta = Dict{Int, CallMeta}
const PC2Effects = Dict{Int, Effects}
const PC2Excts = Dict{Int, Any}

mutable struct CthulhuCacheToken end

struct CthulhuInterpreter <: AbstractInterpreter
    cache_token::CthulhuCacheToken
    native::AbstractInterpreter
    unopt::InferenceDict{InferredSource}
    remarks::InferenceDict{PC2Remarks}
    calls::InferenceDict{PC2CallMeta}
    effects::InferenceDict{PC2Effects}
    exception_types::InferenceDict{PC2Excts}
end

function CthulhuInterpreter(interp::AbstractInterpreter=NativeInterpreter())
    return CthulhuInterpreter(
        CthulhuCacheToken(),
        interp,
        InferenceDict{InferredSource}(),
        InferenceDict{PC2Remarks}(),
        InferenceDict{PC2CallMeta}(),
        InferenceDict{PC2Effects}(),
        InferenceDict{PC2Excts}())
end

Base.show(io::IO, interp::CthulhuInterpreter) = print(io, typeof(interp), "(...)")

CC.InferenceParams(interp::CthulhuInterpreter) = InferenceParams(interp.native)
CC.OptimizationParams(interp::CthulhuInterpreter) =
    OptimizationParams(OptimizationParams(interp.native); preserve_local_sources=true)
CC.get_inference_world(interp::CthulhuInterpreter) = get_inference_world(interp.native)
CC.get_inference_cache(interp::CthulhuInterpreter) = CC.get_inference_cache(interp.native)

CC.may_optimize(::CthulhuInterpreter) = true
CC.may_compress(::CthulhuInterpreter) = false
CC.may_discard_trees(::CthulhuInterpreter) = false
CC.method_table(interp::CthulhuInterpreter) = CC.method_table(interp.native)

# Since the cache for `CthulhuInterpreter` is volatile and does not involve with the
# internal code cache, technically, there's no requirement to supply `cache_owner` as an
# identifier for the internal code cache. However, the definition of `cache_owner` is
# necessary for utilizing the default `CodeInstance` constructor, define the overload here.
CC.cache_owner(interp::CthulhuInterpreter) = interp.cache_token

function get_inference_key(state::InferenceState)
    result = state.result
    CC.is_constproped(state) && return result # TODO result.ci_as_edge?
    isdefined(result, :ci) && return result.ci
    return nothing
end

function CC.add_remark!(interp::CthulhuInterpreter, sv::InferenceState, msg)
    key = get_inference_key(sv)
    key === nothing && return nothing
    push!(get!(PC2Remarks, interp.remarks, key), sv.currpc=>msg)
end

function CC.merge_effects!(interp::CthulhuInterpreter, sv::InferenceState, effects::Effects)
    key = get_inference_key(sv)
    key === nothing && return nothing
    pc2effects = get!(interp.effects, key, PC2Effects())
    old_effects = get(pc2effects, sv.currpc, EFFECTS_TOTAL)
    pc2effects[sv.currpc] = CC.merge_effects(effects, old_effects)
    @invoke CC.merge_effects!(interp::AbstractInterpreter, sv::InferenceState, effects::Effects)
end

function CC.update_exc_bestguess!(interp::CthulhuInterpreter, @nospecialize(exct),
                                  frame::InferenceState)
    key = get_inference_key(frame)
    if key !== nothing
        pc2excts = get!(PC2Excts, interp.exception_types, key)
        old_exct = get(pc2excts, frame.currpc, Union{})
        pc2excts[frame.currpc] = CC.tmerge(CC.typeinf_lattice(interp), exct, old_exct)
    end
    return @invoke CC.update_exc_bestguess!(interp::AbstractInterpreter, exct::Any,
                                            frame::InferenceState)
end

function CC.abstract_call(interp::CthulhuInterpreter, arginfo::CC.ArgInfo, sstate::CC.StmtInfo, sv::InferenceState)
    call = @invoke CC.abstract_call(interp::AbstractInterpreter, arginfo::CC.ArgInfo, sstate::CC.StmtInfo, sv::InferenceState)
    if isa(sv, InferenceState)
        key = get_inference_key(sv)
        if key !== nothing
            CC.Future{Any}(call, interp, sv) do call, interp, sv
                calls = get!(PC2CallMeta, interp.calls, key)
                calls[sv.currpc] = call
                nothing
            end
        end
    end
    return call
end

function InferredSource(state::InferenceState)
    unoptsrc = copy(state.src)
    exct = state.result.exc_result
    return InferredSource(
        unoptsrc,
        copy(state.stmt_info),
        state.ipo_effects,
        state.result.result,
        exct)
end

@static if VERSION ≥ v"1.13-"
function _finishinfer!(frame::InferenceState, interp::CthulhuInterpreter, cycleid::Int, opt_cache::IdDict{MethodInstance, CodeInstance})
    return @invoke CC.finishinfer!(frame::InferenceState, interp::AbstractInterpreter, cycleid::Int, opt_cache::IdDict{MethodInstance, CodeInstance})
end
else
function _finishinfer!(frame::InferenceState, interp::CthulhuInterpreter, cycleid::Int)
    return @invoke CC.finishinfer!(frame::InferenceState, interp::AbstractInterpreter, cycleid::Int)
end
end

function cthulhu_finish(result::Union{Nothing, InferenceResult}, frame::InferenceState, interp::CthulhuInterpreter)
    key = get_inference_key(frame)
    key === nothing && return result
    interp.unopt[key] = InferredSource(frame)

    # Wrap `CallInfo`s with `CthulhuCallInfo`s post-inference.
    calls = get(interp.calls, key, nothing)
    isnothing(calls) && return result
    for (i, info) in enumerate(frame.stmt_info)
        info === NoCallInfo() && continue
        call = get(calls, i, nothing)
        call === nothing && continue
        if isa(info, CC.UnionSplitApplyCallInfo)
            # XXX: `UnionSplitApplyCallInfo` is specially handled in `CC.inline_apply!`,
            # so we can't shove it under a `CthulhuCallInfo`.
            frame.stmt_info[i] = pack_cthulhuinfo_in_unionsplit(call, info)
        else
            frame.stmt_info[i] = CthulhuCallInfo(call)
        end
    end

    return result
end

# Rebuild a `CC.UnionSplitApplyCallInfo` structure where inner `ApplyCallInfo`s wrap a `CthulhuCallInfo`.
# Note that technically, `rt`/`exct`/`effects`/`refinements` are incorrect for each apply call as they
# apply to the union split as a whole, not to individual branches. The idea is simply to preserve them.
function pack_cthulhuinfo_in_unionsplit(call::CallMeta, info::CC.UnionSplitApplyCallInfo)
    infos = CC.ApplyCallInfo[]
    for apply in info.infos
        meta = CallMeta(call.rt, call.exct, call.effects, apply.call, call.refinements)
        push!(infos, CC.ApplyCallInfo(CthulhuCallInfo(meta), apply.arginfo))
    end
    return CC.UnionSplitApplyCallInfo(infos)
end

# Build a `CthulhuCallInfo` structure wrapping `CC.UnionSplitApplyCallInfo`.
function unpack_cthulhuinfo_from_unionsplit(info::CC.UnionSplitApplyCallInfo)
    isempty(info.infos) && return nothing
    apply = info.infos[1]
    isa(apply.call, CthulhuCallInfo) || return nothing
    (; rt, exct, effects, refinements) = apply.call.meta
    infos = CC.ApplyCallInfo[]
    for apply in info.infos
        push!(infos, CC.ApplyCallInfo(apply.call.meta.info, apply.arginfo))
    end
    call = CallMeta(rt, exct, effects, CC.UnionSplitApplyCallInfo(infos), refinements)
    return CthulhuCallInfo(call)
end

function create_cthulhu_source(result::InferenceResult, effects::Effects)
    isa(result.src, OptimizationState) || return result.src
    opt = result.src
    @static if VERSION ≥ v"1.13-"
        optresult = opt.optresult::CC.OptimizationResult
        optresult.simplified || CC.simplify_ir!(optresult)
        opt.src.inlining_cost = CC.compute_inlining_cost(opt.inlining.interp, result, optresult)
        ir = CC.compact!(copy(optresult.ir))
    else
        # get the (theoretically) same effect as the jl_compress_ir -> jl_uncompress_ir -> inflate_ir round-trip
        ir = CC.compact!(CC.cfg_simplify!(CC.copy(opt.ir::IRCode)))
    end
    return OptimizedSource(ir, opt.src, opt.src.inlineable, effects)
end

function set_cthulhu_source!(result::InferenceResult)
    result.src = create_cthulhu_source(result, result.ipo_effects)
end

@static if VERSION ≥ v"1.11-"
# finishinfer! was replaced by finish! in Julia 1.11+
CC.finish!(state::InferenceState, interp::CthulhuInterpreter) = cthulhu_finish(@invoke(CC.finish!(interp::AbstractInterpreter, state::InferenceState)), state, interp)
else
CC.finishinfer!(state::InferenceState, interp::CthulhuInterpreter, cycleid::Int) = cthulhu_finish(_finishinfer!(state, interp, cycleid), state, interp)
end

function CC.finish!(interp::CthulhuInterpreter, caller::InferenceState, validation_world::UInt, time_before::UInt64)
    set_cthulhu_source!(caller.result)
    return @invoke CC.finish!(interp::AbstractInterpreter, caller::InferenceState, validation_world::UInt, time_before::UInt64)
end

function CC.inlining_policy(interp::CthulhuInterpreter,
    @nospecialize(src), @nospecialize(info::CCCallInfo), stmt_flag::UInt32)
    if isa(src, OptimizedSource)
        if CC.is_stmt_inline(stmt_flag) || src.isinlineable
            return true
        end
        return false
    else
        @assert src isa CC.IRCode || src === nothing "invalid Cthulhu code cache"
        # the default inlining policy may try additional effort to find the source in a local cache
        return @invoke CC.inlining_policy(interp::AbstractInterpreter,
            src::Any, info::CCCallInfo, stmt_flag::UInt32)
    end
end
CC.retrieve_ir_for_inlining(cached_result::CodeInstance, src::OptimizedSource) =
    CC.retrieve_ir_for_inlining(cached_result.def, src.ir::IRCode, true)
CC.retrieve_ir_for_inlining(mi::MethodInstance, src::OptimizedSource, preserve_local_sources::Bool) =
    CC.retrieve_ir_for_inlining(mi, src.ir, preserve_local_sources)

function CC.IRInterpretationState(interp::CthulhuInterpreter,
    code::CodeInstance, mi::MethodInstance, argtypes::Vector{Any}, world::UInt)
    inferred = code.inferred
    inferred === nothing && return nothing
    inferred = inferred::OptimizedSource
    ir = CC.copy(inferred.ir)
    src = inferred.src
    spec_info = CC.SpecInfo(src)
    argtypes = CC.va_process_argtypes(CC.optimizer_lattice(interp), argtypes, src.nargs, src.isva)
    return CC.IRInterpretationState(interp, spec_info, ir, mi, argtypes, world,
                                    code.min_world, code.max_world)
end
