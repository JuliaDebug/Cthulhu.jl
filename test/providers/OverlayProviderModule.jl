module OverlayProviderModule # inspired by the Cthulhu integration at serenity4/SPIRV.jl/ext/SPIRVCthulhuExt.jl

using Core.IR

import ..CompilerIntegration
using ..CompilerIntegration: CC, CthulhuInterpreter, OptimizedSource, InferredSource, run_type_inference
using .CC: InferenceResult, OverlayMethodTable, AbstractInterpreter, InferenceResult, InferenceParams, OptimizationParams

using Cthulhu: Cthulhu, AbstractProvider

### Interpreter

Base.Experimental.@MethodTable METHOD_TABLE

macro overlay(ex)
    esc(:(Base.Experimental.@overlay $METHOD_TABLE $ex))
end

@overlay exp(x::Float64) = 42.0

mutable struct OverlayToken end

struct OverlayInterpreter <: AbstractInterpreter
    token::OverlayToken
    method_table::OverlayMethodTable
    local_cache::Vector{InferenceResult}
    world::UInt
    inference_parameters::InferenceParams
    optimization_parameters::OptimizationParams
end

function OverlayInterpreter(world::UInt = Base.get_world_counter();
                            inference_parameters = InferenceParams(),
                            optimization_parameters = OptimizationParams())
    return OverlayInterpreter(
        OverlayToken(),
        OverlayMethodTable(world, METHOD_TABLE),
        InferenceResult[],
        world,
        inference_parameters,
        optimization_parameters,
    )
end

CC.InferenceParams(interp::OverlayInterpreter) = interp.inference_parameters
CC.OptimizationParams(interp::OverlayInterpreter) = interp.optimization_parameters
CC.get_world_counter(interp::OverlayInterpreter) = interp.world
CC.get_inference_cache(interp::OverlayInterpreter) = interp.local_cache
CC.get_inference_world(interp::OverlayInterpreter) = interp.world
CC.cache_owner(interp::OverlayInterpreter) = interp.token
CC.method_table(interp::OverlayInterpreter) = interp.method_table

function Base.show(io::IO, interp::OverlayInterpreter)
  print(io, typeof(interp), "(...)")
end

### Provider

mutable struct OverlayProvider <: AbstractProvider
    interp::OverlayInterpreter
    cthulhu::CthulhuInterpreter
    function OverlayProvider()
        interp = OverlayInterpreter()
        cthulhu = CthulhuInterpreter(interp)
        return new(interp, cthulhu)
    end
end

Cthulhu.get_abstract_interpreter(provider::OverlayProvider) = provider.interp

# Let Cthulhu manage storage of all sources.
# This only works so long as we don't rely on overriding compilation methods for `OverlayInterpreter`
# (besides those from the `AbstractInterpreter` interface that are forwarded by `CthulhuInterpreter`).

CompilerIntegration.run_type_inference(provider::OverlayProvider, interp::OverlayInterpreter, mi::MethodInstance) =
    run_type_inference(provider, provider.cthulhu, mi)
CompilerIntegration.OptimizedSource(provider::OverlayProvider, interp::OverlayInterpreter, ci::CodeInstance) =
    OptimizedSource(provider, provider.cthulhu, ci)
CompilerIntegration.OptimizedSource(provider::OverlayProvider, interp::OverlayInterpreter, result::InferenceResult) =
    OptimizedSource(provider, provider.cthulhu, result)
CompilerIntegration.InferredSource(provider::OverlayProvider, interp::OverlayInterpreter, ci::CodeInstance) =
    InferredSource(provider, provider.cthulhu, ci)

end
