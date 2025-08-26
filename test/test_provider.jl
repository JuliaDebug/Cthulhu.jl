# module test_provider

using Test
using Core.IR
using Cthulhu: Cthulhu, descend, CTHULHU_MODULE
import Compiler # trigger the extension
CthulhuCompilerExt = Base.get_extension(Cthulhu, :CthulhuCompilerExt)
@assert CthulhuCompilerExt !== nothing

@eval module Standard
const Cthulhu = $Cthulhu
using .Cthulhu: DefaultProvider
include("provider_utils.jl")
include("providers/CountingProviderModule.jl")
using .CountingProviderModule: CountingProvider
include("providers/OverlayProviderModule.jl")
using .OverlayProviderModule: OverlayProvider
end

@eval module Ext
const Cthulhu = $CthulhuCompilerExt
using .Cthulhu: DefaultProvider
include("provider_utils.jl")
include("providers/CountingProviderModule.jl")
using .CountingProviderModule: CountingProvider
include("providers/OverlayProviderModule.jl")
using .OverlayProviderModule: OverlayProvider
end

@testset "Example providers" begin
    args = (exp, (Float64,))

    @testset "Provider API" begin
        @test Standard.DefaultProvider !== Ext.DefaultProvider
        Standard.test_provider_api(Standard.DefaultProvider(), args...)
        Standard.test_provider_api(Standard.CountingProvider(), args...)
        Standard.test_provider_api(Standard.OverlayProvider(), args...)
        Ext.test_provider_api(Ext.DefaultProvider(), args...)
        Ext.test_provider_api(Ext.CountingProvider(), args...)
        Ext.test_provider_api(Ext.OverlayProvider(), args...)
    end

    @testset "`descend`" begin
        # FIXME: this hangs
        # Standard.test_descend_for_provider(Standard.DefaultProvider(), args...)
        # Standard.test_descend_for_provider(Standard.CountingProvider(), args...)
        # Standard.test_descend_for_provider(Standard.OverlayProvider(), args...)
        # Ext.test_descend_for_provider(Ext.DefaultProvider(), args...)
        # Ext.test_descend_for_provider(Ext.CountingProvider(), args...)
        # Ext.test_descend_for_provider(Ext.OverlayProvider(), args...)
    end
end

# end # module test_provider
