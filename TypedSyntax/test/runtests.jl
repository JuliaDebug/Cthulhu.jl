using JuliaSyntax: JuliaSyntax, SyntaxNode, children, child, sourcetext, kind, @K_str
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
    for (st, idxsinner, idxsouter) in (
        ("firstfirst(c) = map(x -> first(x), first(c))", (2, 2), (3,)),
        ("""
        firstfirst(c) = map(first(c)) do x
            first(x)
        end
        """, (3, 1), (1, 2))
        )
        rootnode = JuliaSyntax.parse(SyntaxNode, st; filename="TSN3.jl")
        TSN.eval(Expr(rootnode))
        src, _ = getsrc(TSN.firstfirst, (Vector{Vector{Real}},))
        tsn = TypedSyntaxNode(rootnode, src)
        sig, body = children(tsn)
        @test !hastype(child(body, idxsinner...).typ)  # first(x) is hidden in anonymous function and not assignable
        @test child(body, idxsouter...).typ === Vector{Real}
    end

    # `ref` indexing
    st = """
        function setlist!(listset, listget, i, j)
            listset[i+1][j+1] = listget[i][j]
        end
        """
    rootnode = JuliaSyntax.parse(SyntaxNode, st; filename="TSN4.jl")
    TSN.eval(Expr(rootnode))
    src, rt = getsrc(TSN.setlist!, (Vector{Vector{Float32}}, Vector{Vector{UInt8}}, Int, Int))
    tsn = TypedSyntaxNode(rootnode, src)
    sig, body = children(tsn)
    nodelist = child(body, 1, 2, 1, 1)                             # `listget`
    @test sourcetext(nodelist) == "listget" && nodelist.typ === Vector{Vector{UInt8}}
    @test nodelist.parent.typ === Vector{UInt8}                    # `listget[i]`
    @test sourcetext(child(nodelist.parent, 2)) == "i"
    @test nodelist.parent.parent.typ === UInt8                     # `listget[i][j]`

    nodelist = child(body, 1, 1, 1, 1)                             # `listset`
    @test sourcetext(nodelist) == "listset" && nodelist.typ === Vector{Vector{Float32}}
    @test sourcetext(child(nodelist.parent, 2)) == "i+1"
    @test nodelist.parent.typ === Vector{Float32}                  # `listset[i+1]`
    # @test nodelist.parent.parent.typ === nothing                   # `listset[i+1][j+1]`
    @test kind(nodelist.parent.parent.parent) == K"="              # the `setindex!` call
end
