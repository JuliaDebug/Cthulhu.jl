using .CC: AbstractInterpreter, NativeInterpreter, InferenceState, OptimizationState,
    CodeInfo, CodeInstance, InferenceResult, WorldRange, WorldView, IRCode, SSAValue

@static if VERSION ≥ v"1.9-"
    const CCCallInfo = CC.CallInfo
    const NoCallInfo = CC.NoCallInfo
else
    const CCCallInfo = Any
    const NoCallInfo = Nothing
end

struct InferredSource
    src::CodeInfo
    stmt_info::Vector{CCCallInfo}
    effects::Effects
    rt::Any
    InferredSource(src::CodeInfo, stmt_info::Vector{CCCallInfo}, effects, @nospecialize(rt)) =
        new(src, stmt_info, effects, rt)
end

struct OptimizedSource
    ir::IRCode
    src::CodeInfo
    isinlineable::Bool
    effects::Effects
end

const PC2Remarks = Vector{Pair{Int, String}}
const PC2Effects = Dict{Int, Effects}

struct CthulhuInterpreter <: AbstractInterpreter
    native::AbstractInterpreter

    unopt::Dict{Union{MethodInstance,InferenceResult}, InferredSource}
    opt::Dict{MethodInstance, CodeInstance}

    remarks::Dict{Union{MethodInstance,InferenceResult}, PC2Remarks}
    effects::Dict{Union{MethodInstance,InferenceResult}, PC2Effects}
end

function CthulhuInterpreter(interp::AbstractInterpreter=NativeInterpreter())
    return CthulhuInterpreter(
        interp,
        Dict{Union{MethodInstance,InferenceResult}, InferredSource}(),
        Dict{MethodInstance, CodeInstance}(),
        Dict{Union{MethodInstance,InferenceResult}, PC2Remarks}(),
        Dict{Union{MethodInstance,InferenceResult}, PC2Effects}())
end

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
CC.method_table(interp::CthulhuInterpreter) = method_table(interp.native)
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
    key = CC.any(sv.result.overridden_by_const) ? sv.result : sv.linfo
    push!(get!(PC2Remarks, interp.remarks, key), sv.currpc=>msg)
end

@static if VERSION ≥ v"1.9-"
function CC.merge_effects!(interp::CthulhuInterpreter, sv::InferenceState, effects::Effects)
    key = CC.any(sv.result.overridden_by_const) ? sv.result : sv.linfo
    pc2effects = get!(interp.effects, key, PC2Effects())
    pc2effects[sv.currpc] = CC.merge_effects(get!(pc2effects, sv.currpc, EFFECTS_TOTAL), effects)
    @invoke CC.merge_effects!(interp::AbstractInterpreter, sv::InferenceState, effects::Effects)
end
end

@static if VERSION ≤ v"1.10.0-DEV.221"
function CC.type_annotate!(interp::CthulhuInterpreter, sv::InferenceState, run_optimizer::Bool)
    changemap = @invoke CC.type_annotate!(interp::AbstractInterpreter, sv::InferenceState, run_optimizer::Bool)
    changemap === nothing && return nothing
    key = CC.any(sv.result.overridden_by_const) ? sv.result : sv.linfo
    pc2remarks = get(interp.remarks, key, nothing)
    if pc2remarks !== nothing
        sort!(pc2remarks)
        unique!(pc2remarks)
        for (idx, v) in enumerate(changemap)
            if v == typemin(Int)
                for i = searchsorted(pc2remarks, idx=>"", by=((idx,msg),)->idx)
                    @assert false "remarks found in unreached region"
                end
            end
        end
        for (idx, v) in enumerate(changemap)
            if v < 0
                for i = searchsorted(pc2remarks, idx=>"", by=((idx,msg),)->idx)
                    pc2remarks[i] = pc2remarks[i].first+v => pc2remarks[i].second
                end
            end
        end
    end
    pc2effects = get(interp.effects, key, nothing)
    if pc2effects !== nothing
        for (idx, v) in enumerate(changemap)
            if v == typemin(Int)
                delete!(pc2effects, idx)
            end
        end
        for (idx, v) in enumerate(changemap)
            if v < 0
                haskey(pc2effects, idx) || continue
                pc2effects[idx+v] = pc2effects[idx]
                delete!(pc2effects, idx)
            end
        end
    end
    return changemap
end
end

function InferredSource(state::InferenceState)
    unoptsrc = copy(state.src)
    @static if VERSION < v"1.10.0-DEV.1033"
        # xref: https://github.com/JuliaLang/julia/pull/49378
        unoptsrc.slottypes = let slottypes = unoptsrc.slottypes
            slottypes === nothing ? nothing : copy(slottypes)
        end
    end
    return InferredSource(
        unoptsrc,
        copy(state.stmt_info),
        isdefined(CC, :Effects) ? state.ipo_effects : nothing,
        state.result.result)
end

function CC.finish(state::InferenceState, interp::CthulhuInterpreter)
    res = @invoke CC.finish(state::InferenceState, interp::AbstractInterpreter)
    key = CC.any(state.result.overridden_by_const) ? state.result : state.linfo
    interp.unopt[key] = InferredSource(state)
    return res
end

function create_cthulhu_source(@nospecialize(x), effects::Effects)
    isa(x, OptimizationState) || return x
    ir = x.ir::IRCode
    return OptimizedSource(ir, x.src, x.src.inlineable, effects)
end

@static if VERSION ≥ v"1.9-"
function CC.transform_result_for_cache(interp::CthulhuInterpreter,
    linfo::MethodInstance, valid_worlds::WorldRange, result::InferenceResult)
    return create_cthulhu_source(result.src, result.ipo_effects)
end
else
function CC.transform_result_for_cache(interp::CthulhuInterpreter,
    linfo::MethodInstance, valid_worlds::WorldRange, @nospecialize(inferred_result),
    ipo_effects::CC.Effects)
    return create_cthulhu_source(inferred_result, ipo_effects)
end
end

@static if VERSION ≥ v"1.9-"
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
else
function CC.inlining_policy(interp::CthulhuInterpreter,
    @nospecialize(src), stmt_flag::UInt8, mi::MethodInstance, argtypes::Vector{Any})
    @assert isa(src, OptimizedSource) || isnothing(src)
    if isa(src, OptimizedSource)
        if CC.is_stmt_inline(stmt_flag) || src.isinlineable
            return src.ir
        end
    else
        # the default inlining policy may try additional effor to find the source in a local cache
        return @invoke CC.inlining_policy(interp::AbstractInterpreter,
            nothing, stmt_flag::UInt8, mi::MethodInstance, argtypes::Vector{Any})
    end
    return nothing
end
end

@static if isdefined(CC, :AbsIntState)
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
elseif VERSION ≥ v"1.9-"
function CC.codeinst_to_ir(interp::CthulhuInterpreter, code::CodeInstance)
    inferred = @atomic :monotonic code.inferred
    inferred === nothing && return nothing
    inferred = inferred::OptimizedSource
    return CC.copy(inferred.ir)
end
end

function CC.finish!(interp::CthulhuInterpreter, caller::InferenceResult)
    caller.src = create_cthulhu_source(caller.src, caller.ipo_effects)
end
