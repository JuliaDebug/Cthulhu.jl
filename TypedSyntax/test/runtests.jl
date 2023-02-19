using JuliaSyntax: JuliaSyntax, SyntaxNode, children, child
using TypedSyntax: TypedSyntax, TypedSyntaxNode, NotFound, getsrc
using Test

hastype(@nospecialize(T)) = isa(T, Type) && T !== NotFound

module TSN end

@testset "TypedSyntax.jl" begin
    st = """
    f(x, y, z) = x * y + z
    """
    rootnode = JuliaSyntax.parse(SyntaxNode, st; filename="TSN1.jl")
    TSN.eval(Expr(rootnode))
    src, _ = getsrc(TSN.f, (Float32, Int, Float64))
    tsn = TypedSyntaxNode(rootnode, src)
    sig, body = children(tsn)
    @test children(sig)[2].typ === Float32
    @test children(sig)[3].typ === Int
    @test children(sig)[4].typ === Float64
    @test body.typ === Float64   # aggregate output
    @test children(body)[1].typ === Float32

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
    sig, body = children(tsn)
    @test length(children(sig)) == 4
    @test children(body)[2].typ === Int32

    # Mapping ambiguity
    for (st, idxs) in (
        ("firstfirst(c) = map(x -> first(x), first(c))", (2, 2)),
        ("""
        firstfirst(c) = map(first(c)) do x
            first(x)
        end
        """, (3, 1))
        )
        rootnode = JuliaSyntax.parse(SyntaxNode, st; filename="TSN3.jl")
        TSN.eval(Expr(rootnode))
        src, _ = getsrc(TSN.firstfirst, (Vector{Any},))
        tsn = TypedSyntaxNode(rootnode, src)
        sig, body = children(tsn)
        @test !hastype(child(body, idxs...).typ)  # first(x) is hidden in anonymous function and not assignable
    end
end
