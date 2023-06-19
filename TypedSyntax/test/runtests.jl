using Dates, InteractiveUtils, Test

@testset "TypedSyntax.jl" begin
    @testset "test_TypedSyntax.jl" begin
        include("test_TypedSyntax.jl")
    end
end

if parse(Bool, get(ENV, "CI", "false"))
    include("exhaustive.jl")
end

module VSCodeServer
    struct InlineDisplay
        is_repl::Bool
    end
    const INLAY_HINTS_ENABLED = Ref(true)

    displayed_output = IOBuffer()

    function Base.display(d::InlineDisplay, x)
        println(displayed_output, x)
    end
end
module TestVSCodeExt # stops modules defined in test files from overwriting stuff from previous test
using Dates, InteractiveUtils, Test, ..VSCodeServer

@testset "VSCode TypedSyntax.jl" begin
    @testset "vscode_test_TypedSyntax.jl" begin
        include("test_TypedSyntax.jl")
    end
    @testset "test_vscode.jl" begin
        include("test_vscode.jl")
    end
end
end