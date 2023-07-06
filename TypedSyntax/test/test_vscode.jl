module test_vscode

using ..VSCodeServer
using TypedSyntax: TypedSyntax, TypedSyntaxNode, getsrc, InlayHint, WarnUnstable
using Dates, InteractiveUtils, Test

include("test_module.jl")

@testset "test_vscode.jl" begin
    # VSCode
    tsn = TypedSyntaxNode(TSN.fVSCode, (Int64,))

    type_hints = Dict{String, Vector{InlayHint}}()
    warn_diagnostics = WarnUnstable[]
    printstyled(devnull, tsn, type_hints, warn_diagnostics)
    @test occursin(r"""TypedSyntax.WarnUnstable\[TypedSyntax.WarnUnstable\([^\)]+\), TypedSyntax.WarnUnstable\([^\)]+\)\]\nDict\{String, Vector\{TypedSyntax.InlayHint\}\}\(\\"[^"]+\\" => \[TypedSyntax.InlayHint\(\d+, \d+, \\"::Union\{Float64, Int64\}\\", nothing\), TypedSyntax.InlayHint\(\d+, \d+, \\"\(\\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \\"\)::Union\{Float64, Int64\}\\", nothing\)""", String(take!(VSCodeServer.displayed_output)))
    @show getproperty.(first(values(type_hints)), :label)
    @test getproperty.(first(values(type_hints)), :kind) == [nothing, 1, nothing] && getproperty.(first(values(type_hints)), :label) == ["::Union{Float64, Int64}", "(", ")::Union{Float64, Int64}"]
    @test length(warn_diagnostics) == 2

    type_hints = Dict{String, Vector{InlayHint}}()
    warn_diagnostics = WarnUnstable[]
    printstyled(devnull, tsn, type_hints, warn_diagnostics; hide_type_stable=false)
    @test occursin(r"""TypedSyntax.WarnUnstable\[TypedSyntax.WarnUnstable\([^\)]+\), TypedSyntax.WarnUnstable\([^\)]+\)\]\nDict{String, Vector{TypedSyntax.InlayHint}}\(\\"[^"]+\\" => \[TypedSyntax.InlayHint\(\d+, \d+, \"::Int64\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"::Union{Float64, Int64}\", nothing\), TypedSyntax.InlayHint\(\d+, \d+, \"::Int64\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"\(\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"::Int64\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"\)::Int64\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"::Int64\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"\(\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"::Int64\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"\)::Int64\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"\(\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"::Int64\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"\(\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"::Int64\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"\)::Bool\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"::Int64\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"::Float64\", 1\), TypedSyntax.InlayHint\(\d+, \d+, \"\)::Union{Float64, Int64}\", nothing\)\]\)\n""", String(take!(VSCodeServer.displayed_output)))
    @test getproperty.(first(values(type_hints)), :kind) == vcat(1, nothing, repeat([1], 15), nothing) && getproperty.(first(values(type_hints)), :label) == ["::Int64"
    "::Union{Float64, Int64}"
    "::Int64"
    "("
    "::Int64"
    ")::Int64"
    "::Int64"
    "("
    "::Int64"
    ")::Int64"
    "("
    "::Int64"
    "("
    "::Int64"
    ")::Bool"
    "::Int64"
    "::Float64"
    ")::Union{Float64, Int64}"]
    @test length(warn_diagnostics) == 2
end
end