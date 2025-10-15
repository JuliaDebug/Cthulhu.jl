module test_provider

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
@static if VERSION > v"1.13-"
    include("providers/ExternalProviderModule.jl")
    using .ExternalProviderModule: ExternalProvider
end
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
@static if VERSION > v"1.13-"
    include("providers/ExternalProviderModule.jl")
    using .ExternalProviderModule: ExternalProvider
end
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

impl_warnings_noopt = impl_warnings[[3, 7, 9, 11, 13, 15]]

function test_function(x)
    y = 1 + 1 # constant propgation (concrete evaluation)
    z, state = iterate((2, x)) # semi-concrete evaluation
    return exp(x + z) # a normal call
end

@testset "Example providers" begin
    args = (test_function, (Int,))

    @testset "Provider API" begin
        @test Standard.DefaultProvider !== Ext.DefaultProvider
        Standard.test_provider_api(Standard.DefaultProvider(), args...)
        Standard.test_provider_api(Standard.CountingProvider(), args...)
        Standard.test_provider_api(Standard.OverlayProvider(), args...)
        VERSION > v"1.13-" && Standard.test_provider_api(Standard.ExternalProvider(), args...)
        Ext.test_provider_api(Ext.DefaultProvider(), args...)
        Ext.test_provider_api(Ext.CountingProvider(), args...)
        Ext.test_provider_api(Ext.OverlayProvider(), args...)
        VERSION > v"1.13-" && Ext.test_provider_api(Ext.ExternalProvider(), args...)
    end

    @testset "`descend`" begin
        @test_logs normal_warnings... Standard.test_descend_for_provider(Standard.DefaultProvider(), args...)
        @test_logs impl_warnings... Standard.test_descend_for_provider(Standard.CountingProvider(), args...)
        @test_logs impl_warnings... Standard.test_descend_for_provider(Standard.OverlayProvider(), args...)
        VERSION > v"1.13-" && @test_logs impl_warnings_noopt... Standard.test_descend_for_provider(Standard.ExternalProvider(), args...)
        @test_logs normal_warnings... Ext.test_descend_for_provider(Ext.DefaultProvider(), args...)
        @test_logs impl_warnings... Ext.test_descend_for_provider(Ext.CountingProvider(), args...)
        @test_logs impl_warnings... Ext.test_descend_for_provider(Ext.OverlayProvider(), args...)
        VERSION > v"1.13-" && @test_logs impl_warnings_noopt... Ext.test_descend_for_provider(Ext.ExternalProvider(), args...)
    end
end;

end # module test_provider
