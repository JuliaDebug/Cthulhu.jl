# module test_provider

using Test
using Core.IR
using Cthulhu: Cthulhu, descend, CTHULHU_MODULE
import Compiler # trigger the extension
CthulhuCompilerExt = Base.get_extension(Cthulhu, :CthulhuCompilerExt)
@assert CthulhuCompilerExt !== nothing

@eval module Standard
const Cthulhu = $Cthulhu
using Cthulhu: descend
using .Cthulhu: DefaultProvider
include("provider_utils.jl")
include("providers/CountingProviderModule.jl")
using .CountingProviderModule: CountingProvider
include("providers/OverlayProviderModule.jl")
using .OverlayProviderModule: OverlayProvider
end

@eval module Ext
const Cthulhu = $CthulhuCompilerExt
using Cthulhu: descend
using .Cthulhu: DefaultProvider
include("provider_utils.jl")
include("providers/CountingProviderModule.jl")
using .CountingProviderModule: CountingProvider
include("providers/OverlayProviderModule.jl")
using .OverlayProviderModule: OverlayProvider
end

logs(warnings) = tuple.(:warn, warnings)

normal_warnings = logs([
    "Disable optimization to see the inference remarks.",
    "Enable optimization to see the inlining costs.",
])

impl_warnings = logs([
    r"Remarks could not be retrieved",
    r"Remarks could not be retrieved",
    r"Effects could not be retrieved",
    normal_warnings[1][end],
    r"Effects could not be retrieved",
    r"Effects could not be retrieved",
    normal_warnings[2][end],
    r"Remarks could not be retrieved",
    r"Effects could not be retrieved",
    r"Remarks could not be retrieved",
    r"Effects could not be retrieved",
    r"Remarks could not be retrieved",
    r"Effects could not be retrieved",
    r"Remarks could not be retrieved",
    r"Effects could not be retrieved",
])

@testset "Example providers" begin
    args = (gcd, (Int, Int))

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
        @test_logs normal_warnings... Standard.test_descend_for_provider(Standard.DefaultProvider(), args...)
        @test_logs impl_warnings... Standard.test_descend_for_provider(Standard.CountingProvider(), args...)
        @test_logs impl_warnings... Standard.test_descend_for_provider(Standard.OverlayProvider(), args...)
        @test_logs normal_warnings... Ext.test_descend_for_provider(Ext.DefaultProvider(), args...)
        @test_logs impl_warnings... Ext.test_descend_for_provider(Ext.CountingProvider(), args...)
        @test_logs impl_warnings... Ext.test_descend_for_provider(Ext.OverlayProvider(), args...)
    end
end;

# end # module test_provider
