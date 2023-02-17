using JuliaSyntax: JuliaSyntax, SyntaxNode
using TypedSyntax: TypedSyntax, TypedSyntaxNode, getsrc
using Test

module TSN end

@testset "TypedSyntax.jl" begin
    st = """
    f(x, y, z) = x * y + z
    """
    rootnode = JuliaSyntax.parse(SyntaxNode, st; filename="TSN1.jl")
    TSN.eval(Expr(rootnode))
    src, _ = getsrc(TSN.f, (Float32, Int, Float64))
    tsn = TypedSyntaxNode(rootnode, src)
    sig = tsn.children[1]
    @test sig.children[2].typ === Float32
    @test sig.children[3].typ === Int
    @test sig.children[4].typ === Float64
    body = tsn.children[2]
    @test body.typ === Float64   # aggregate output
    @test body.children[1].typ === Float32

    # Multiline
    st = """
    function g(a, b, c)
        x = a + b
        return x + c
    end
    """
    rootnode = JuliaSyntax.parse(SyntaxNode, st; filename="TSN2.jl")
    TSN.eval(Expr(rootnode))
    src, _ = getsrc(TSN.g, (Int16, Int16, Int32))
    tsn = TypedSyntaxNode(rootnode, src)
    sig, body = tsn.children
    @test length(sig.children) == 4
    @test body.children[2].typ === Int32
end
