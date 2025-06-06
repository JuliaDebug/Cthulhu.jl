module test_AbstractInterpreter

using Test, Cthulhu
if isdefined(parentmodule(@__MODULE__), :VSCodeServer)
    using ..VSCodeServer
end

@doc """
    @newinterp NewInterpreter [ephemeral_cache::Bool=false]

Defines new `NewInterpreter <: AbstractInterpreter` whose cache is separated
from the native code cache, satisfying the minimum interface requirements.

When the `ephemeral_cache=true` option is specified, `NewInterpreter` will hold
`CodeInstance` in an ephemeral non-integrated cache, rather than in the integrated
`Core.Compiler.InternalCodeCache`.
Keep in mind that ephemeral cache lacks support for invalidation and doesn't persist across
sessions. However it is an usual Julia object of the type `code_cache::IdDict{MethodInstance,CodeInstance}`,
making it easier for debugging and inspecting the compiler behavior.
"""
macro newinterp(InterpName, ephemeral_cache::Bool=false)
    cache_token = QuoteNode(gensym(string(InterpName, "CacheToken")))
    InterpCacheName = esc(Symbol(string(InterpName, "Cache")))
    InterpName = esc(InterpName)
    C = Core
    CC = Cthulhu.CC
    quote
        $(ephemeral_cache && quote
        struct $InterpCacheName
            dict::IdDict{$C.MethodInstance,$C.CodeInstance}
        end
        $InterpCacheName() = $InterpCacheName(IdDict{$C.MethodInstance,$C.CodeInstance}())
        end)
        struct $InterpName <: $CC.AbstractInterpreter
            meta # additional information
            world::UInt
            inf_params::$CC.InferenceParams
            opt_params::$CC.OptimizationParams
            inf_cache::Vector{$CC.InferenceResult}
            $(ephemeral_cache && :(code_cache::$InterpCacheName))
            function $InterpName(meta = nothing;
                                    world::UInt = Base.get_world_counter(),
                                    inf_params::$CC.InferenceParams = $CC.InferenceParams(),
                                    opt_params::$CC.OptimizationParams = $CC.OptimizationParams(),
                                    inf_cache::Vector{$CC.InferenceResult} = $CC.InferenceResult[],
                                    $(ephemeral_cache ?
                                    Expr(:kw, :(code_cache::$InterpCacheName), :($InterpCacheName())) :
                                    Expr(:kw, :_, :nothing)))
                return $(ephemeral_cache ?
                    :(new(meta, world, inf_params, opt_params, inf_cache, code_cache)) :
                    :(new(meta, world, inf_params, opt_params, inf_cache)))
            end
        end
        $CC.InferenceParams(interp::$InterpName) = interp.inf_params
        $CC.OptimizationParams(interp::$InterpName) = interp.opt_params
        $CC.get_inference_world(interp::$InterpName) = interp.world
        $CC.get_inference_cache(interp::$InterpName) = interp.inf_cache
        $CC.cache_owner(::$InterpName) = $cache_token
        $(ephemeral_cache && quote
        $CC.code_cache(interp::$InterpName) = $CC.WorldView(interp.code_cache, $CC.WorldRange(interp.world))
        $CC.get(wvc::$CC.WorldView{$InterpCacheName}, mi::$C.MethodInstance, default) = get(wvc.cache.dict, mi, default)
        $CC.getindex(wvc::$CC.WorldView{$InterpCacheName}, mi::$C.MethodInstance) = getindex(wvc.cache.dict, mi)
        $CC.haskey(wvc::$CC.WorldView{$InterpCacheName}, mi::$C.MethodInstance) = haskey(wvc.cache.dict, mi)
        $CC.setindex!(wvc::$CC.WorldView{$InterpCacheName}, ci::$C.CodeInstance, mi::$C.MethodInstance) = setindex!(wvc.cache.dict, ci, mi)
        end)
    end
end

const CC = Cthulhu.CC

# `OverlayMethodTable`
# --------------------
using Base.Experimental: @MethodTable, @overlay

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
