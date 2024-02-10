module test_AbstractInterpreter

using Test, Cthulhu
if isdefined(parentmodule(@__MODULE__), :VSCodeServer)
    using ..VSCodeServer
end

const CC = Core.Compiler

if isdefined(CC, :cache_owner)
macro newinterp(InterpName)
    InterpCacheName = QuoteNode(Symbol(string(InterpName, "Cache")))
    InterpName = esc(InterpName)
    C = Core
    CC = Core.Compiler
    quote
        struct $InterpName <: $CC.AbstractInterpreter
            meta # additional information
            world::UInt
            inf_params::$CC.InferenceParams
            opt_params::$CC.OptimizationParams
            inf_cache::Vector{$CC.InferenceResult}
            function $InterpName(meta = nothing;
                                 world::UInt = Base.get_world_counter(),
                                 inf_params::$CC.InferenceParams = $CC.InferenceParams(),
                                 opt_params::$CC.OptimizationParams = $CC.OptimizationParams(),
                                 inf_cache::Vector{$CC.InferenceResult} = $CC.InferenceResult[])
                return new(meta, world, inf_params, opt_params, inf_cache)
            end
        end
        $CC.InferenceParams(interp::$InterpName) = interp.inf_params
        $CC.OptimizationParams(interp::$InterpName) = interp.opt_params
        $CC.get_inference_world(interp::$InterpName) = interp.world
        $CC.get_inference_cache(interp::$InterpName) = interp.inf_cache
        $CC.cache_owner(::$InterpName) = $InterpCacheName
    end
end
else
macro newinterp(InterpName)
    InterpCacheName = esc(Symbol(string(InterpName, "Cache")))
    InterpName = esc(InterpName)
    C = Core
    CC = Core.Compiler
    quote
        struct $InterpCacheName
            dict::IdDict{$C.MethodInstance,$C.CodeInstance}
        end
        $InterpCacheName() = $InterpCacheName(IdDict{$C.MethodInstance,$C.CodeInstance}())
        struct $InterpName <: $CC.AbstractInterpreter
            meta # additional information
            world::UInt
            inf_params::$CC.InferenceParams
            opt_params::$CC.OptimizationParams
            inf_cache::Vector{$CC.InferenceResult}
            code_cache::$InterpCacheName
            function $InterpName(meta = nothing;
                                 world::UInt = Base.get_world_counter(),
                                 inf_params::$CC.InferenceParams = $CC.InferenceParams(),
                                 opt_params::$CC.OptimizationParams = $CC.OptimizationParams(),
                                 inf_cache::Vector{$CC.InferenceResult} = $CC.InferenceResult[],
                                 code_cache::$InterpCacheName = $InterpCacheName())
                return new(meta, world, inf_params, opt_params, inf_cache, code_cache)
            end
        end
        $CC.InferenceParams(interp::$InterpName) = interp.inf_params
        $CC.OptimizationParams(interp::$InterpName) = interp.opt_params
        @static if VERSION ≥ v"1.11.0-DEV.1498"
        CC.get_inference_world(interp::$InterpName) = interp.world
        else
        CC.get_world_counter(interp::$InterpName) = interp.world
        end
        $CC.get_inference_cache(interp::$InterpName) = interp.inf_cache
        $CC.code_cache(interp::$InterpName) = $CC.WorldView(interp.code_cache, $CC.WorldRange(interp.world))
        $CC.get(wvc::$CC.WorldView{$InterpCacheName}, mi::$C.MethodInstance, default) = get(wvc.cache.dict, mi, default)
        $CC.getindex(wvc::$CC.WorldView{$InterpCacheName}, mi::$C.MethodInstance) = getindex(wvc.cache.dict, mi)
        $CC.haskey(wvc::$CC.WorldView{$InterpCacheName}, mi::$C.MethodInstance) = haskey(wvc.cache.dict, mi)
        $CC.setindex!(wvc::$CC.WorldView{$InterpCacheName}, ci::$C.CodeInstance, mi::$C.MethodInstance) = setindex!(wvc.cache.dict, ci, mi)
    end
end
end # isdefined(CC, :cache_owner)

@doc """
@newinterp NewInterpreter

Defines new `NewInterpreter <: AbstractInterpreter` whose cache is separated
from the native code cache, satisfying the minimum interface requirements.
""" var"@newinterp"

# `OverlayMethodTable`
# --------------------
import Base.Experimental: @MethodTable, @overlay

@newinterp MTOverlayInterp
@MethodTable OverlayedMT
CC.method_table(interp::MTOverlayInterp) =
    CC.OverlayMethodTable(Cthulhu.get_inference_world(interp), OverlayedMT)
@overlay OverlayedMT sin(x::Float64) = 1

@testset "OverlayMethodTable integration" begin
    interp, mi = Cthulhu.mkinterp((Int,); interp=MTOverlayInterp()) do x
        sin(x)
    end
    inferred = interp.unopt[mi]
    @test inferred.rt === Core.Const(1)
end

end # module test_AbstractInterpreter
