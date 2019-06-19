struct Callsite
    id::Int # ssa-id
    info::CallInfo
end
canreflect(call::Callsite) = canreflect(call.info)
reflect(call::Callsite; optimize=true, params=current_params()) = reflect(call.info, optimize=optimize, params=params)

## CallInfo's that Cthulhu handles separately

# Constructor of a task
struct TaskCallInfo <: CallInfo
    task_ci::CallInfo
end
canreflect(ci::TaskCallInfo) = canreflect(ci.task_ci)
reflect(ci::TaskCallInfo; kwargs...) = reflect(ci.task_ci; kwargs...)

# Special handling for ReturnTypeCall
struct ReturnTypeCallInfo <: CallInfo
    called_ci::CallInfo
end
canreflect(ci::ReturnTypeCallInfo) = canreflect(ci.called_ci)
reflect(ci::ReturnTypeCallInfo; kwargs...) = reflect(ci.called_ci; kwargs...)

# CUDA callsite
struct CuCallInfo <: CallInfo
    cuda_ci::CallInfo
end
canreflect(ci::CuCallInfo) = canreflect(ci.cuda_ci)
reflect(ci::CuCallInfo; kwargs...) = reflect(ci.cuda_ci; kwargs...)

import TypedCodeUtils: lookthrough, identify_invoke, identify_call,
                                    process_invoke, process_call, DefaultConsumer

function find_callsites(ref::Reflection)
    invokes = filter((c)->lookthrough(identify_invoke,      c), ref.CI.code)
    calls   = filter((c)->lookthrough(identify_call,        c), ref.CI.code)

    invokes = map((arg) -> process_invoke(DefaultConsumer(), ref, arg...), invokes)
    calls   = map((arg) -> process_call(  DefaultConsumer(), ref, arg...), calls)

    callsites = append!(invokes, calls)
    sort!(callsites, by=(c)->first(c))
    callsites = map(c->Callsite(c...), callsites)
    return callsites
end


