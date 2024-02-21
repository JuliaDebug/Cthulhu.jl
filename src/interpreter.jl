using .CC: AbstractInterpreter, NativeInterpreter, InferenceState, OptimizationState,
    CodeInfo, CodeInstance, InferenceResult, WorldRange, WorldView, IRCode, SSAValue,
    CallInfo as CCCallInfo, NoCallInfo

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

const InferenceKey = Union{MethodInstance,InferenceResult}
const InferenceDict{T} = IdDict{InferenceKey, T}
const OptimizationDict = IdDict{MethodInstance, CodeInstance}
const PC2Remarks = Vector{Pair{Int, String}}
const PC2Effects = Dict{Int, Effects}
const PC2Excts = Dict{Int, Any}

struct CthulhuInterpreter <: AbstractInterpreter
    native::AbstractInterpreter

    unopt::InferenceDict{InferredSource}
    opt::OptimizationDict

    remarks::InferenceDict{PC2Remarks}
    effects::InferenceDict{PC2Effects}
    exception_types::InferenceDict{PC2Excts}
end

function CthulhuInterpreter(interp::AbstractInterpreter=NativeInterpreter())
    return CthulhuInterpreter(
        interp,
        InferenceDict{InferredSource}(),
        OptimizationDict(),
        InferenceDict{PC2Remarks}(),
        InferenceDict{PC2Effects}(),
        InferenceDict{PC2Excts}())
end

import .CC: InferenceParams, OptimizationParams, #=get_inference_world,=#
    get_inference_cache, code_cache, lock_mi_inference, unlock_mi_inference, method_table,
    inlining_policy
using Base: @invoke

CC.InferenceParams(interp::CthulhuInterpreter) = InferenceParams(interp.native)
@static if VERSION ≥ v"1.11.0-DEV.851"
CC.OptimizationParams(interp::CthulhuInterpreter) =
    OptimizationParams(OptimizationParams(interp.native); preserve_local_sources=true)
else
CC.OptimizationParams(interp::CthulhuInterpreter) = OptimizationParams(interp.native)
end
#=CC.=#get_inference_world(interp::CthulhuInterpreter) = get_inference_world(interp.native)
CC.get_inference_cache(interp::CthulhuInterpreter) = get_inference_cache(interp.native)

CC.may_optimize(interp::CthulhuInterpreter) = true
CC.may_compress(interp::CthulhuInterpreter) = false
CC.may_discard_trees(interp::CthulhuInterpreter) = false
CC.verbose_stmt_info(interp::CthulhuInterpreter) = true

CC.method_table(interp::CthulhuInterpreter) = method_table(interp.native)

@static if VERSION ≥ v"1.11.0-DEV.1552"
# Since the cache for `CthulhuInterpreter` is volatile and does not involve with the
# internal code cache, technically, there's no requirement to supply `cache_owner` as an
# identifier for the internal code cache. However, the definition of `cache_owner` is
# necessary for utilizing the default `CodeInstance` constructor, define the overload here.
struct CthulhuCacheToken
    token
end
CC.cache_owner(interp::CthulhuInterpreter) = CthulhuCacheToken(CC.cache_owner(interp.native))
end

struct CthulhuCache
    cache::OptimizationDict
end
CC.code_cache(interp::CthulhuInterpreter) = WorldView(CthulhuCache(interp.opt), WorldRange(get_inference_world(interp)))
CC.get(wvc::WorldView{CthulhuCache}, mi::MethodInstance, default) = get(wvc.cache.cache, mi, default)
CC.haskey(wvc::WorldView{CthulhuCache}, mi::MethodInstance) = haskey(wvc.cache.cache, mi)
CC.setindex!(wvc::WorldView{CthulhuCache}, ci::CodeInstance, mi::MethodInstance) = setindex!(wvc.cache.cache, ci, mi)

function CC.add_remark!(interp::CthulhuInterpreter, sv::InferenceState, msg)
    key = CC.any(sv.result.overridden_by_const) ? sv.result : sv.linfo
    push!(get!(PC2Remarks, interp.remarks, key), sv.currpc=>msg)
end

function CC.merge_effects!(interp::CthulhuInterpreter, sv::InferenceState, effects::Effects)
    key = CC.any(sv.result.overridden_by_const) ? sv.result : sv.linfo
    pc2effects = get!(interp.effects, key, PC2Effects())
    pc2effects[sv.currpc] = CC.merge_effects(get!(pc2effects, sv.currpc, EFFECTS_TOTAL), effects)
    @invoke CC.merge_effects!(interp::AbstractInterpreter, sv::InferenceState, effects::Effects)
end

function InferredSource(state::InferenceState)
    unoptsrc = copy(state.src)
    exct = @static VERSION ≥ v"1.11.0-DEV.207" ? state.result.exc_result : nothing
    return InferredSource(
        unoptsrc,
        copy(state.stmt_info),
        state.ipo_effects,
        state.result.result,
        exct)
end

function CC.finish(state::InferenceState, interp::CthulhuInterpreter)
    res = @invoke CC.finish(state::InferenceState, interp::AbstractInterpreter)
    key = CC.any(state.result.overridden_by_const) ? state.result : state.linfo
    interp.unopt[key] = InferredSource(state)
    return res
end

function create_cthulhu_source(@nospecialize(opt), effects::Effects)
    isa(opt, OptimizationState) || return opt
    @static if VERSION > v"1.10"
        # get the (theoretically) same effect as the jl_compress_ir -> jl_uncompress_ir -> inflate_ir round-trip
        ir = CC.compact!(CC.cfg_simplify!(CC.copy(opt.ir::IRCode)))
    else
        # TODO do the round-trip here?
        ir = CC.copy(opt.ir::IRCode)
    end
    return OptimizedSource(ir, opt.src, opt.src.inlineable, effects)
end

function CC.transform_result_for_cache(interp::CthulhuInterpreter,
    linfo::MethodInstance, valid_worlds::WorldRange, result::InferenceResult)
    return create_cthulhu_source(result.src, result.ipo_effects)
end

@static if VERSION ≥ v"1.11.0-DEV.879"
function CC.inlining_policy(interp::CthulhuInterpreter,
    @nospecialize(src), @nospecialize(info::CCCallInfo), stmt_flag::UInt32)
    if isa(src, OptimizedSource)
        if CC.is_stmt_inline(stmt_flag) || src.isinlineable
            return src.ir
        end
    else
        @assert src isa CC.IRCode || src === nothing "invalid Cthulhu code cache"
        # the default inlining policy may try additional effor to find the source in a local cache
        return @invoke CC.inlining_policy(interp::AbstractInterpreter,
            src::Any, info::CCCallInfo, stmt_flag::UInt32)
    end
    return nothing
end
else
function CC.inlining_policy(interp::CthulhuInterpreter,
    @nospecialize(src), @nospecialize(info::CCCallInfo),
    stmt_flag::(@static VERSION ≥ v"1.11.0-DEV.377" ? UInt32 : UInt8),
    mi::MethodInstance, argtypes::Vector{Any})
    if isa(src, OptimizedSource)
        if CC.is_stmt_inline(stmt_flag) || src.isinlineable
            return src.ir
        end
    else
        @assert src isa CC.SemiConcreteResult || src === nothing "invalid Cthulhu code cache"
        # the default inlining policy may try additional effor to find the source in a local cache
        return @invoke CC.inlining_policy(interp::AbstractInterpreter,
            src::Any, info::CCCallInfo,
            stmt_flag::(@static VERSION ≥ v"1.11.0-DEV.377" ? UInt32 : UInt8),
            mi::MethodInstance, argtypes::Vector{Any})
    end
    return nothing
end
end

function CC.IRInterpretationState(interp::CthulhuInterpreter,
    code::CodeInstance, mi::MethodInstance, argtypes::Vector{Any}, world::UInt)
    inferred = @atomic :monotonic code.inferred
    inferred === nothing && return nothing
    inferred = inferred::OptimizedSource
    ir = CC.copy(inferred.ir)
    src = inferred.src
    method_info = CC.MethodInfo(src)
    return CC.IRInterpretationState(interp, method_info, ir, mi, argtypes, world,
                                    src.min_world, src.max_world)
end

@static if VERSION ≥ v"1.11.0-DEV.737"
function CC.finish!(interp::CthulhuInterpreter, caller::InferenceState)
    result = caller.result
    opt = result.src
    result.src = create_cthulhu_source(opt, result.ipo_effects)
    if opt isa CC.OptimizationState
        CC.ir_to_codeinf!(opt)
    end
    return nothing
end
else
function CC.finish!(interp::CthulhuInterpreter, caller::InferenceResult)
    caller.src = create_cthulhu_source(caller.src, caller.ipo_effects)
end
end

@static if VERSION ≥ v"1.11.0-DEV.1127"
function CC.update_exc_bestguess!(interp::CthulhuInterpreter, @nospecialize(exct),
                                  frame::InferenceState)
    key = CC.any(frame.result.overridden_by_const) ? frame.result : frame.linfo
    pc2excts = get!(PC2Excts, interp.exception_types, key)
    pc2excts[frame.currpc] = CC.tmerge(CC.typeinf_lattice(interp), exct, get(pc2excts, frame.currpc, Union{}))
    return @invoke CC.update_exc_bestguess!(interp::AbstractInterpreter, exct::Any,
                                            frame::InferenceState)
end
end
