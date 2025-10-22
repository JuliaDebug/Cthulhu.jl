module test_provider

using Test
using Core.IR
using Cthulhu: CTHULHU_MODULE

@eval module Impl
const Cthulhu = $(CTHULHU_MODULE[])
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
        Impl.test_provider_api(Impl.DefaultProvider(), args...)
        Impl.test_provider_api(Impl.CountingProvider(), args...)
        Impl.test_provider_api(Impl.OverlayProvider(), args...)
        VERSION > v"1.13-" && Impl.test_provider_api(Impl.ExternalProvider(), args...)
    end

    @testset "`descend`" begin
        @test_logs normal_warnings... Impl.test_descend_for_provider(Impl.DefaultProvider(), args...)
        @test_logs impl_warnings... Impl.test_descend_for_provider(Impl.CountingProvider(), args...)
        @test_logs impl_warnings... Impl.test_descend_for_provider(Impl.OverlayProvider(), args...)
        VERSION > v"1.13-" && @test_logs impl_warnings_noopt... Impl.test_descend_for_provider(Impl.ExternalProvider(), args...)
    end
end;

end # module test_provider
