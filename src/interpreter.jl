using Core.Compiler: AbstractInterpreter, NativeInterpreter, InferenceState,
    OptimizationState, CodeInfo, CodeInstance, InferenceResult

struct InferredSource
    src::CodeInfo
    stmt_infos::Vector{Any}
    rt::Any
end

mutable struct CthulhuInterpreter <: AbstractInterpreter
    native::NativeInterpreter

    unopt::Dict{MethodInstance, InferredSource}
    opt::Dict{MethodInstance, CodeInstance}

    msgs::Dict{MethodInstance, Vector{Pair{Int, String}}}
end

CthulhuInterpreter() = CthulhuInterpreter(
    NativeInterpreter(),
    Dict{MethodInstance, CodeInfo}(),
    Dict{MethodInstance, CodeInfo}(),
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
Core.Compiler.verbose_stmt_info(ni::CthulhuInterpreter) = true

function Core.Compiler.add_remark!(ei::CthulhuInterpreter, sv::InferenceState, msg)
    push!(get!(ei.msgs, sv.linfo, Tuple{Int, String}[]),
        sv.currpc => msg)
end

function Core.Compiler.finish(state::InferenceState, ei::CthulhuInterpreter)
    r = invoke(Core.Compiler.finish, Tuple{InferenceState, AbstractInterpreter}, state, ei)
    ei.unopt[state.linfo] = InferredSource(
        copy(isa(state.src, OptimizationState) ?
            state.src.src : state.src),
        copy(state.stmt_info),
        state.result.result)
    return r
end
