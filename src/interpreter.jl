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
const PC2Effects = Dict{Int, Effects}
const PC2Excts = Dict{Int, Any}

mutable struct CthulhuCacheToken end

struct CthulhuInterpreter <: AbstractInterpreter
    cache_token::CthulhuCacheToken
    native::AbstractInterpreter
    unopt::InferenceDict{InferredSource}
    remarks::InferenceDict{PC2Remarks}
    effects::InferenceDict{PC2Effects}
    exception_types::InferenceDict{PC2Excts}
end

function CthulhuInterpreter(interp::AbstractInterpreter=NativeInterpreter())
    return CthulhuInterpreter(
        CthulhuCacheToken(),
        interp,
        InferenceDict{InferredSource}(),
        InferenceDict{PC2Remarks}(),
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

function cthulhu_finish(@specialize(finishfunc), state::InferenceState, interp::CthulhuInterpreter, cycleid::Int)
    res = @invoke finishfunc(state::InferenceState, interp::AbstractInterpreter, cycleid::Int)
    key = get_inference_key(state)
    if key !== nothing
        interp.unopt[key] = InferredSource(state)
    end
    return res
end

function create_cthulhu_source(@nospecialize(opt), effects::Effects)
    isa(opt, OptimizationState) || return opt
    @static if VERSION ≥ v"1.13-"
        result = opt.optresult::CC.OptimizationResult
        result.simplified || CC.simplify_ir!(result)
        ir = CC.compact!(copy(result.ir))
    else
        # get the (theoretically) same effect as the jl_compress_ir -> jl_uncompress_ir -> inflate_ir round-trip
        ir = CC.compact!(CC.cfg_simplify!(CC.copy(opt.ir::IRCode)))
    end
    return OptimizedSource(ir, opt.src, opt.src.inlineable, effects)
end

function set_cthulhu_source!(result::InferenceResult)
    result.src = create_cthulhu_source(result.src, result.ipo_effects)
end

CC.finishinfer!(state::InferenceState, interp::CthulhuInterpreter, cycleid::Int) = cthulhu_finish(CC.finishinfer!, state, interp, cycleid)
function CC.finish!(interp::CthulhuInterpreter, caller::InferenceState, validation_world::UInt, time_before::UInt64)
    set_cthulhu_source!(caller.result)
    return @invoke CC.finish!(interp::AbstractInterpreter, caller::InferenceState, validation_world::UInt, time_before::UInt64)
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
        # the default inlining policy may try additional effor to find the source in a local cache
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
