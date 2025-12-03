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

Base.show(io::IO, interp::CthulhuInterpreter) = print(io, typeof(interp), '(', interp.native, ')')

CC.InferenceParams(interp::CthulhuInterpreter) = InferenceParams(interp.native)
CC.OptimizationParams(interp::CthulhuInterpreter) =
    OptimizationParams(OptimizationParams(interp.native); preserve_local_sources=true)
CC.get_inference_world(interp::CthulhuInterpreter) = CC.get_inference_world(interp.native)
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

function OptimizedSource(provider::AbstractProvider, interp::CthulhuInterpreter, override::InferenceResult)
    if isa(override.src, OptimizedSource)
        return override.src
    end
    # On Julia 1.14+, InferenceResult from MethodMatchInfo.call_results may not have
    # been processed by our CC.finish! hook. Try to create the source if possible.
    src = create_cthulhu_source(override, override.ipo_effects)
    if isa(src, OptimizedSource)
        return src
    end
    error("couldn't find the source")
end

function OptimizedSource(provider::AbstractProvider, interp::CthulhuInterpreter, ci::CodeInstance)
    opt = ci.inferred
    isa(opt, OptimizedSource) && return opt
    @eval Main begin
        interp = $interp
        ci = $ci
        opt = $opt
    end
    error("couldn't find the source; you may inspect `Main.interp|ci|opt`")
end

function InferredSource(provider::AbstractProvider, interp::CthulhuInterpreter, src::Union{CodeInstance,InferenceResult})
    unopt = get(interp.unopt, src, nothing)
    if unopt !== nothing
        return unopt
    end
    # On Julia 1.14+, InferenceResult from MethodMatchInfo.call_results may not have
    # been processed by our hooks. Try to construct InferredSource from the result.
    if isa(src, InferenceResult)
        result_src = src.src
        if isa(result_src, CodeInfo)
            # Create a minimal InferredSource from available data
            return InferredSource(
                copy(result_src),
                Any[NoCallInfo() for _ in 1:length(result_src.code)],
                src.ipo_effects,
                src.result,
                src.exc_result)
        elseif isa(result_src, OptimizedSource)
            # The source has already been optimized - extract CodeInfo from it
            return InferredSource(
                copy(result_src.src),
                Any[NoCallInfo() for _ in 1:length(result_src.src.code)],
                src.ipo_effects,
                src.result,
                src.exc_result)
        end
    end
    error("couldn't find the unoptimized source for $src")
end

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

function CC.abstract_call(interp::CthulhuInterpreter, arginfo::CC.ArgInfo, sstate::CC.StatementState, sv::InferenceState)
    call = @invoke CC.abstract_call(interp::AbstractInterpreter, arginfo::CC.ArgInfo, sstate::CC.StatementState, sv::InferenceState)
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
        collect(Any, state.stmt_info),
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
    src = CC.ir_to_codeinf!(opt)
    return OptimizedSource(ir, src, src.inlineable, effects)
end

function set_cthulhu_source!(result::InferenceResult)
    result.src = create_cthulhu_source(result, result.ipo_effects)
end

@static if VERSION ≥ v"1.13-"
CC.finishinfer!(state::InferenceState, interp::CthulhuInterpreter, cycleid::Int, opt_cache::IdDict{MethodInstance, CodeInstance}) = cthulhu_finish(_finishinfer!(state, interp, cycleid, opt_cache), state, interp)
else
CC.finishinfer!(state::InferenceState, interp::CthulhuInterpreter, cycleid::Int) = cthulhu_finish(_finishinfer!(state, interp, cycleid), state, interp)
end

function CC.finish!(interp::CthulhuInterpreter, caller::InferenceState, validation_world::UInt, time_before::UInt64)
    # On Julia 1.14+, transform_result_for_cache is called inside finish! which would
    # overwrite result.src. We need to save our OptimizedSource and restore it after.
    set_cthulhu_source!(caller.result)
    our_src = caller.result.src
    result = @invoke CC.finish!(interp::AbstractInterpreter, caller::InferenceState, validation_world::UInt, time_before::UInt64)
    # Restore our OptimizedSource if it was overwritten
    if isa(our_src, OptimizedSource)
        caller.result.src = our_src
    end
    return result
end

function CC.src_inlining_policy(interp::CthulhuInterpreter,
    @nospecialize(src), @nospecialize(info::CCCallInfo), stmt_flag::UInt32)
    if isa(src, OptimizedSource)
        if CC.is_stmt_inline(stmt_flag) || src.isinlineable
            return true
        end
        return false
    else
        @assert src isa CC.IRCode || src === nothing "invalid Cthulhu code cache"
        # the default inlining policy may try additional effort to find the source in a local cache
        return @invoke CC.src_inlining_policy(interp::AbstractInterpreter,
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
