using Test, PerformanceTestTools

@testset "runtests.jl" begin
    @testset "test_Cthulhu.jl" begin
        include("test_Cthulhu.jl")
    end

    @testset "test_codeview.jl" begin
        include("test_codeview.jl")
    end

    # TODO enable this test on nightly
    @static if VERSION < v"1.9.0-DEV.1295"
        @testset "test_irshow.jl" begin
            include("test_irshow.jl")
        end
    else
        @info "skipped test_irshow.jl"
    end

    @testset "test_terminal.jl" begin
        include("test_terminal.jl")
    end

    @testset "test_AbstractInterpreter.jl" begin
        include("test_AbstractInterpreter.jl")
    end
end
