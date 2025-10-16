using Test, PerformanceTestTools
using Core: Const # allows correct printing as `Const` instead of `Core.Const`
using Cthulhu: is_compiler_loaded, is_compiler_extension_loaded

if is_compiler_loaded() # don't load it otherwise, as that would load the Cthulhu extension
    using Revise
    Revise.track(Base) # get the `@info` log now, to avoid polluting test outputs later
end

@testset "Cthulhu.jl" begin
    before = is_compiler_extension_loaded()
    @testset "Core functionality" include("test_Cthulhu.jl")
    @testset "Code view" include("test_codeview.jl")
    @testset "Provider functionality" include("test_provider.jl")
    @testset "Terminal tests" include("test_terminal.jl")
    @testset "AbstractInterpreter" include("test_AbstractInterpreter.jl")
    after = is_compiler_extension_loaded()
    @assert before === after # make sure we don't mess up the test setup by loading Compiler durin tests
    if !is_compiler_extension_loaded()
        @eval import Compiler # trigger the extension
        if is_compiler_extension_loaded() # allow extension to be disabled locally during development
            @testset "Tests with Compiler extension loaded" begin
                @testset "Core functionality" include("test_Cthulhu.jl")
                @testset "Code view" include("test_codeview.jl")
                @testset "Provider functionality" include("test_provider.jl")
                @testset "Terminal tests" include("test_terminal.jl")
                @testset "AbstractInterpreter" include("test_AbstractInterpreter.jl")
            end
        end
    end
    # TODO enable these tests
    false || return @info "skipped test_irshow.jl"
    @testset "IRShow display tests" include("test_irshow.jl")
end;

# TODO enable the VSCode-related tests

# module VSCodeServer
#     using TypedSyntax

#     struct InlineDisplay
#         is_repl::Bool
#     end
#     const INLAY_HINTS_ENABLED = Ref(true)
#     const DIAGNOSTICS_ENABLED = Ref(true)

#     inlay_hints = []
#     diagnostics = []

#     function Base.display(d::InlineDisplay, x)
#         if x isa Dict{String, Vector{TypedSyntax.InlayHint}}
#             push!(inlay_hints, x)
#         elseif eltype(x) == TypedSyntax.Diagnostic
#             push!(diagnostics, x)
#         end
#         return nothing
#     end

#     function reset_test_containers()
#         empty!(inlay_hints)
#         empty!(diagnostics)
#     end
# end
# module TestVSCodeExt # stops modules defined in test files from overwriting stuff from previous test
# using Test, PerformanceTestTools, ..VSCodeServer
# @testset "runtests.jl VSCodeExt" begin
#     @testset "test_Cthulhu.jl" begin
#         include("test_Cthulhu.jl")
#     end

#     @testset "test_codeview.jl" begin
#         include("test_codeview.jl")
#         include("test_codeview_vscode.jl")
#     end

#     # TODO enable these tests
#     if false
#         @testset "test_irshow.jl" begin
#             include("test_irshow.jl")
#         end
#     else
#         @info "skipped test_irshow.jl"
#     end
#     if false
#         @testset "test_terminal.jl" begin
#             include("test_terminal.jl")
#         end
#     else
#         @info "skipped test_terminal.jl"
#     end

#     @testset "test_AbstractInterpreter.jl" begin
#         include("test_AbstractInterpreter.jl")
#     end
# end
# end
