using JuliaSyntax: JuliaSyntax, SyntaxNode, children, child, sourcetext, kind, @K_str
using TypedSyntax: TypedSyntax, TypedSyntaxNode, getsrc
using Test

has_name_typ(node, name::Symbol, @nospecialize(T)) = kind(node) == K"Identifier" && node.val === name && node.typ === T

module TSN

function has2xa(x)
    x &= x
end
function has2xb(x)
    x -= x
    return x
end

# This is taken from the definition of `sin(::Int)` in Base, copied here for testing purposes
# in case the implementation changes
for f in (:mysin,)
    @eval function ($f)(x::Real)
        xf = float(x)
        x === xf && throw(MethodError($f, (x,)))
        return ($f)(xf)
    end
end

function summer(list)
    s = 0                    # deliberately ::Int to test type-changes
    for x in list
        s += x
    end
    return s
end

zerowhere(::AbstractArray{T}) where T<:Real = zero(T)

# with two uses of the same slot in the same call
function simplef(a, b)
    z = a * a
    return z + b + y
end

add2(x) = x[1] + x[2]

likevect(X::T...) where {T} = T[ X[i] for i = 1:length(X) ]

end

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
    # Check that `x` gets an assigned type
    nodex = child(body, 1, 1)
    @test nodex.typ === Int16

    # Target ambiguity
    st = "math(x) = x + sin(x + π / 4)"
    rootnode = JuliaSyntax.parse(SyntaxNode, st; filename="TSN2.jl")
    TSN.eval(Expr(rootnode))
    src, _ = getsrc(TSN.math, (Int,))
    tsn = TypedSyntaxNode(rootnode, src)
    sig, body = children(tsn)
    @test has_name_typ(child(body, 1), :x, Int)
    @test has_name_typ(child(body, 3, 2, 1), :x, Int)
    pi4 = child(body, 3, 2, 3)
    @test kind(pi4) == K"call" && pi4.typ == Core.Const(π / 4)
    tsn = TypedSyntaxNode(TSN.has2xa, (Real,))
    @test tsn.typ === Any
    sig, body = children(tsn)
    @test has_name_typ(child(sig, 2), :x, Real)
    @test has_name_typ(child(body, 1, 2), :x, Real)
    @test has_name_typ(child(body, 1, 1), :x, Any)
    tsn = TypedSyntaxNode(TSN.has2xb, (Real,))
    @test tsn.typ === Any
    sig, body = children(tsn)
    @test has_name_typ(child(sig, 2), :x, Real)
    @test has_name_typ(child(body, 1, 2), :x, Real)
    @test has_name_typ(child(body, 1, 1), :x, Any)

    # Target duplication
    st = "math2(x) = sin(x) + sin(x)"
    rootnode = JuliaSyntax.parse(SyntaxNode, st; filename="TSN2.jl")
    TSN.eval(Expr(rootnode))
    src, _ = getsrc(TSN.math2, (Int,))
    tsn = TypedSyntaxNode(rootnode, src)
    sig, body = children(tsn)
    @test body.typ === Float64
    @test_broken child(body, 1).typ === Float64
    tsn = TypedSyntaxNode(TSN.simplef, Tuple{Float32, Int32})
    sig, body = children(tsn)
    @test has_name_typ(child(body, 1, 2, 1), :a, Float32)
    @test has_name_typ(child(body, 1, 2, 3), :a, Float32)

    # Inner functions
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
        @test child(body, idxsinner...).typ === nothing
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
    @test kind(nodelist.parent.parent.parent) == K"="              # the `setindex!` call

    # tuple-destructuring
    st = """
        function callfindmin(list)
            val, idx = findmin(list)
            x, y = idx, val
            return y
        end
    """
    rootnode = JuliaSyntax.parse(SyntaxNode, st; filename="TSN5.jl")
    TSN.eval(Expr(rootnode))
    src, rt = getsrc(TSN.callfindmin, (Vector{Float64},))
    tsn = TypedSyntaxNode(rootnode, src)
    sig, body = children(tsn)
    t = child(body, 1, 1)
    @test kind(t) == K"tuple"
    @test has_name_typ(child(t, 1), :val, Float64)
    @test has_name_typ(child(t, 2), :idx, Int)
    t = child(body, 2, 1)
    @test kind(t) == K"tuple"
    @test has_name_typ(child(t, 1), :x, Int)
    @test has_name_typ(child(t, 2), :y, Float64)

    # kwfuncs
    st = """
    function avoidzero(x; avoid_zero=true)
        fx = float(x)
        return iszero(x) ? oftype(fx, NaN) : fx
    end
    """
    rootnode = JuliaSyntax.parse(SyntaxNode, st; filename="TSN6.jl")
    TSN.eval(Expr(rootnode))
    src, rt = getsrc(TSN.avoidzero, (Int,))
    # src looks like this:
    #   %1 = Main.TSN.:(var"#avoidzero#6")(true, #self#, x)::Float64
    #        return %1
    # Consequently there is nothing to match, but at least we shouldn't error
    tsn = TypedSyntaxNode(rootnode, src)
    @test isa(tsn, TypedSyntaxNode)
    @test rt === Float64
    # Try the kwbodyfunc
    m = which(TSN.avoidzero, (Int,))
    src, rt = getsrc(Base.bodyfunction(m), (Bool, typeof(TSN.avoidzero), Int,))
    tsn = TypedSyntaxNode(rootnode, src)
    sig, body = children(tsn)
    isz = child(body, 2, 1, 1)
    @test kind(isz) == K"call" && child(isz, 1).val == :iszero
    @test isz.typ === Bool
    @test child(body, 2, 1, 2).typ == Core.Const(NaN)

    # macros in function definition
    tsn = TypedSyntaxNode(TSN.mysin, (Int,))
    @test kind(tsn) == K"macrocall"
    sig, body = children(child(tsn, 2))
    @test has_name_typ(child(sig, 2, 1), :x, Int)
    @test has_name_typ(child(body, 1, 1), :xf, Float64)

    # `for` loops
    tsn = TypedSyntaxNode(TSN.summer, (Vector{Float64},))
    @test tsn.typ == Union{Int,Float64}
    sig, body = children(tsn)
    @test has_name_typ(child(sig, 2), :list, Vector{Float64})
    @test_broken has_name_typ(child(body, 1, 1), :s, Int)
    @test_broken has_name_typ(child(body, 2, 1, 1), :x, Float64)
    node = child(body, 2, 2, 1)
    @test kind(node) == K"+="
    @test has_name_typ(child(node, 1), :s, Float64)   # if this line runs, the LHS now has type `Float64`
    @test has_name_typ(child(node, 2), :x, Float64)
    @test has_name_typ(child(body, 3, 1), :s, Union{Float64, Int})

    # `where` and unnamed arguments
    tsn = TypedSyntaxNode(TSN.zerowhere, (Vector{Int16},))
    sig, body = children(tsn)
    @test child(sig, 1, 2).typ === Vector{Int16}
    @test body.typ === Core.Const(Int16(0))

    # varargs
    tsn = TypedSyntaxNode(TSN.likevect, (Int, Int))
    sig, body = children(tsn)
    nodeva = child(sig, 1, 2)
    @test kind(nodeva) == K"..."
    @test has_name_typ(child(nodeva, 1, 1), :X, Tuple{Int,Int})

    # Display
    tsn = TypedSyntaxNode(TSN.summer, (Vector{Any},))
    str = sprint(tsn; context=:color=>true) do io, obj
        printstyled(io, obj; iswarn=true, hide_type_stable=false)
    end
    @test occursin("summer(list\e[36m::Vector{Any}\e[39m)\e[31m::Any", str)
    @test occursin("s\e[31m::Any\e[39m += x\e[31m::Any\e[39m", str)
    str = sprint(tsn; context=:color=>true) do io, obj
        printstyled(io, obj; type_annotations=false)
    end
    @test occursin("summer(list)", str)
    @test occursin("s += x", str)
    tsn = TypedSyntaxNode(TSN.zerowhere, (Vector{Int16},))
    str = sprint(tsn; context=:color=>true) do io, obj
        printstyled(io, obj; iswarn=true, hide_type_stable=false)
    end
    @test occursin("AbstractArray{T}\e[36m::Vector{Int16}\e[39m", str)
    @test occursin("Real\e[36m::Int16\e[39m", str)
    tsn = TypedSyntaxNode(TSN.add2, (Vector{Float32},))
    str = sprint(tsn; context=:color=>true) do io, obj
        printstyled(io, obj; iswarn=true, hide_type_stable=false)
    end
    @test occursin("[1]\e[36m::Float32\e[39m", str)
    @test occursin("[2]\e[36m::Float32\e[39m", str)
end
