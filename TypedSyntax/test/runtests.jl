using JuliaSyntax: JuliaSyntax, SyntaxNode, children, child, sourcetext, kind, @K_str
using TypedSyntax: TypedSyntax, TypedSyntaxNode, getsrc
using Dates, InteractiveUtils, Test

has_name_typ(node, name::Symbol, @nospecialize(T)) = kind(node) == K"Identifier" && node.val === name && node.typ === T
has_name_notyp(node, name::Symbol) = has_name_typ(node, name, nothing)

include("test_module.jl")

@testset "TypedSyntax.jl" begin
    include("tests.jl")
end

if parse(Bool, get(ENV, "CI", "false"))
    include("exhaustive.jl")
end

module VSCodeServer
@testset "TypedSyntax.jl VSCodeExt" begin
    include("tests.jl")
end