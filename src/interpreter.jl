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
    Dict{MethodInstance, Vector{Tuple{Int, String}}}()
)

import Core.Compiler: InferenceParams, OptimizationParams, get_world_counter,
    get_inference_cache, code_cache,
    WorldView, lock_mi_inference, unlock_mi_inference, InferenceState

InferenceParams(ei::CthulhuInterpreter) = InferenceParams(ei.native)
OptimizationParams(ei::CthulhuInterpreter) = OptimizationParams(ei.native)
get_world_counter(ei::CthulhuInterpreter) = get_world_counter(ei.native)
get_inference_cache(ei::CthulhuInterpreter) = get_inference_cache(ei.native)

# No need to do any locking since we're not putting our results into the runtime cache
lock_mi_inference(ei::CthulhuInterpreter, mi::MethodInstance) = nothing
unlock_mi_inference(ei::CthulhuInterpreter, mi::MethodInstance) = nothing

code_cache(ei::CthulhuInterpreter) = ei.opt
Core.Compiler.get(a::Dict, b, c) = Base.get(a,b,c)
Core.Compiler.get(a::WorldView{<:Dict}, b, c) = Base.get(a.cache,b,c)
Core.Compiler.haskey(a::Dict, b) = Base.haskey(a, b)
Core.Compiler.haskey(a::WorldView{<:Dict}, b) =
    Core.Compiler.haskey(a.cache, b)
Core.Compiler.setindex!(a::Dict, b, c) = setindex!(a, b, c)
Core.Compiler.may_optimize(ei::CthulhuInterpreter) = true
Core.Compiler.may_compress(ei::CthulhuInterpreter) = false
Core.Compiler.may_discard_trees(ei::CthulhuInterpreter) = false
Core.Compiler.verbose_stmt_info(ei::CthulhuInterpreter) = true

function Core.Compiler.add_remark!(ei::CthulhuInterpreter, sv::InferenceState, msg)
    push!(get!(ei.msgs, sv.linfo, Tuple{Int, String}[]),
        sv.currpc => msg)
end

function Core.Compiler.finish(state::InferenceState, ei::CthulhuInterpreter)
    r = invoke(Core.Compiler.finish, Tuple{InferenceState, AbstractInterpreter}, state, ei)
    ei.unopt[Core.Compiler.any(state.result.overridden_by_const) ? state.result : state.linfo] = InferredSource(
        copy(isa(state.src, OptimizationState) ?
            state.src.src : state.src),
        copy(state.stmt_info),
        state.result.result)
    return r
end

function Core.Compiler.transform_result_for_cache(interp::CthulhuInterpreter, linfo::MethodInstance,
        valid_worlds::Core.Compiler.WorldRange, @nospecialize(inferred_result))
    if isa(inferred_result, OptimizationState)
        opt = inferred_result
        if isdefined(opt, :ir)
            return OptimizedSource(opt.ir, inlining_policy(interp.native)(opt.src) !== nothing)
        end
    end
    return inferred_result
end

function Core.Compiler.inlining_policy(interp::CthulhuInterpreter)
    function (src)
        @assert isa(src, OptimizedSource)
        return src.isinlineable ? src.ir : nothing
    end
end

function Core.Compiler.finish!(interp::CthulhuInterpreter, caller::InferenceResult)
    if isa(caller.src, OptimizationState)
        opt = caller.src
        if isdefined(opt, :ir)
            caller.src = OptimizedSource(opt.ir, inlining_policy(interp.native)(opt.src) !== nothing)
        end
    end
end
