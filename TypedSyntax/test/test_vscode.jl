module test_vscode

using ..VSCodeServer
using TypedSyntax: TypedSyntax, TypedSyntaxNode, getsrc
using Dates, InteractiveUtils, Test

include("test_module.jl")

@testset "test_vscode.jl" begin
    # VSCode
    tsn = TypedSyntaxNode(TSN.fVSCode, (Int64,))
    printstyled(devnull, tsn)
    @test occursin(r"""TypedSyntax.WarnUnstable\[TypedSyntax.WarnUnstable\([^\)]+\), TypedSyntax.WarnUnstable\([^\)]+\)\]\nDict\{String, Vector\{TypedSyntax.InlayHint\}\}\(\\"[^"]+\\" => \[TypedSyntax.InlayHint\(\d+, \d+, \\"::Union\{Float64, Int64\}\\", nothing\), TypedSyntax.InlayHint\(\d+, \d+, \\"\(\\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \\"\)::Union\{Float64, Int64\}\\", nothing\)""", String(take!(VSCodeServer.displayed_output)))

    printstyled(devnull, tsn; hide_type_stable=false)
    @test occursin(r"""TypedSyntax.WarnUnstable\[TypedSyntax.WarnUnstable\([^\)]+\), TypedSyntax.WarnUnstable\([^\)]+\)\]\nDict{String, Vector{TypedSyntax.InlayHint}}\(\\"[^"]+\\" => \[TypedSyntax.InlayHint\(\d+, \d+, \"::Int64\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"::Union{Float64, Int64}\", nothing\), TypedSyntax.InlayHint\(\d+, \d+, \"::Int64\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"\(\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"::Int64\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"\)::Int64\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"::Int64\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"\(\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"::Int64\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"\)::Int64\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"\(\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"::Int64\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"\(\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"::Int64\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"\)::Bool\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"::Int64\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"::Float64\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"\)::Union{Float64, Int64}\", nothing\)\]\)\n""", String(take!(VSCodeServer.displayed_output)))
end
end