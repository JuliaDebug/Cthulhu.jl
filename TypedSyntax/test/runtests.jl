using JuliaSyntax: JuliaSyntax, SyntaxNode, children, child, sourcetext, kind, @K_str
using TypedSyntax: TypedSyntax, TypedSyntaxNode, getsrc
using Dates, InteractiveUtils, Test

has_name_typ(node, name::Symbol, @nospecialize(T)) = kind(node) == K"Identifier" && node.val === name && node.typ === T
has_name_notyp(node, name::Symbol) = has_name_typ(node, name, nothing)

include("test_module.jl")

@testset "TypedSyntax.jl" begin
    specializations(m::Method) = isdefined(Base, :specializations) ? Base.specializations(m) : m.specializations

    st = """
    f(x, y, z) = x * y + z
    """
    rootnode = JuliaSyntax.parsestmt(SyntaxNode, st; filename="TSN1.jl")
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
    rootnode = JuliaSyntax.parsestmt(SyntaxNode, st; filename="TSN2.jl")
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
    rootnode = JuliaSyntax.parsestmt(SyntaxNode, st; filename="TSN2.jl")
    TSN.eval(Expr(rootnode))
    src, _ = getsrc(TSN.math, (Int,))
    tsn = TypedSyntaxNode(rootnode, src)
    sig, body = children(tsn)
    @test has_name_typ(child(body, 1), :x, Int)
    @test has_name_typ(child(body, 3, 2, 1), :x, Int)
    pi4 = child(body, 3, 2, 3)
    @test kind(pi4) == K"call" && pi4.typ == typeof(π / 4)
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
    rootnode = JuliaSyntax.parsestmt(SyntaxNode, st; filename="TSN2.jl")
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
        rootnode = JuliaSyntax.parsestmt(SyntaxNode, st; filename="TSN3.jl")
        TSN.eval(Expr(rootnode))
        src, _ = getsrc(TSN.firstfirst, (Vector{Vector{Real}},))
        tsn = TypedSyntaxNode(rootnode, src)
        sig, body = children(tsn)
        @test child(body, idxsinner...).typ === nothing
        @test child(body, idxsouter...).typ === Vector{Real}
    end

    # body macros
    tsn = TypedSyntaxNode(TSN.hasmacro, (Tuple{Int,Int}, Char))
    sig, body = children(tsn)
    @test has_name_typ(child(body, 2, 2, 2), :t, Tuple{Int,Int})
    @test has_name_typ(child(body, 2, 3), :x, Char)

    # signature macros
    tsn = TypedSyntaxNode(TSN.nospec, (Any,))
    sig, body = children(tsn)
    node = child(sig, 2)
    @test kind(node) == K"macrocall"
    @test child(node, 1).val == Symbol("@nospecialize")
    @test has_name_typ(child(node, 2), :x, Any)
    @test body.typ === Any
    tsn = TypedSyntaxNode(TSN.nospec2, (AbstractVecOrMat,))
    sig, body = children(tsn)
    node = child(sig, 2)
    @test kind(node) == K"macrocall"
    @test child(node, 1).val == Symbol("@nospecialize")
    arg = child(node, 2)
    @test kind(arg) == K"::"
    @test has_name_typ(child(arg, 1), :x, AbstractVecOrMat)
    @test body.typ === Any
    tsn = TypedSyntaxNode(eltype, (TSN.ReadOnly,))
    @test tsn.typ === Type{Int}

    # signature return-type annotations
    tsn = TypedSyntaxNode(TSN.withrt, (IO,))
    @test tsn.typ === Bool
    sig, body = children(tsn)
    @test has_name_typ(child(sig, 1, 2, 1), :io, IO)
    tsn = TypedSyntaxNode(TSN.mytimes, (Bool,Float16))
    sig, body = children(tsn)
    @test has_name_typ(child(sig, 1, 1, 2, 1), :x, Bool)
    @test has_name_typ(child(sig, 1, 1, 3, 1), :y, Float16)

    # operators
    tsn = TypedSyntaxNode(+, (TSN.MyInt, TSN.MyInt))
    sig, body = children(tsn)
    @test has_name_typ(child(sig, 2, 1), :a, TSN.MyInt)
    tsn = TypedSyntaxNode(-, (TSN.MyInt, TSN.MyInt))
    sig, body = children(tsn)
    @test has_name_typ(child(sig, 2, 1), :a, TSN.MyInt)

    # `ref` indexing
    st = """
        function setlist!(listset, listget, i, j)
            listset[i+1][j+1] = listget[i][j]
        end
        """
    rootnode = JuliaSyntax.parsestmt(SyntaxNode, st; filename="TSN4.jl")
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
    rootnode = JuliaSyntax.parsestmt(SyntaxNode, st; filename="TSN5.jl")
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

    tsn = TypedSyntaxNode(TSN.bar381, (TSN.Foo381,))
    sig, body = children(tsn)
    lhs = child(body, 1, 1)
    @test has_name_typ(child(lhs, 1), :a, Any)
    @test has_name_typ(child(lhs, 2, 1), :b1, Any)
    @test has_name_typ(child(lhs, 2, 2), :b2, Any)

    tsn = TypedSyntaxNode(TSN.extrema2, (Tuple{Int,Int}, Tuple{Int,Int}))
    sig, body = children(tsn)
    @test child(sig, 2).typ == Tuple{Int,Int}

    g = Base.Generator(identity, 1.0:4.0)
    tsn = TypedSyntaxNode(TSN.typeof_first_item, (typeof(g),))
    sig, body = children(tsn)
    @test has_name_typ(child(body, 3, 1, 1), :val, Float64)

    # GlobalRefs
    tsn = TypedSyntaxNode(TSN.getchar1, (Int,))
    sig, body = children(tsn)
    @test has_name_typ(child(body, 1), :charset1, Any)
    tsn = TypedSyntaxNode(TSN.getchar2, (Int,))
    sig, body = children(tsn)
    @test has_name_typ(child(body, 1), :charset2, typeof(TSN.charset2))

    # Generators & comprehensions
    tsn = TypedSyntaxNode(TSN.boxedgenerator368, (Int,))
    sig, body = children(tsn)
    cnode = child(body, 2)
    @test kind(cnode) == K"comprehension"
    @test cnode.typ == Vector
    tsn = TypedSyntaxNode(TSN.nestedgenerators, (Int, Int))
    sig, body = children(tsn)
    @test kind(body) == K"generator"
    @test body.typ <: Base.Iterators.Flatten
    tsn = TypedSyntaxNode(TSN.nestedgenerators, (Int,))
    sig, body = children(tsn)
    @test kind(body) == K"generator"
    @test body.typ <: Base.Iterators.Flatten
    tsn = TypedSyntaxNode(TSN.nestedexplicit, (Int,))
    sig, body = children(tsn)
    @test kind(body) == K"comprehension"
    @test body.typ <: Vector
    node = child(body, 1)
    @test kind(node) == K"generator"
    @test node.typ <: Base.Generator

    # Broadcasting
    tsn = TypedSyntaxNode(TSN.fbroadcast, (Vector{Int},))
    sig, body = children(tsn)
    @test body.typ === Float64
    cnode = child(body, 2)
    @test kind(cnode) == K"dotcall"
    @test cnode.typ == Vector{Float64}
    tsn = TypedSyntaxNode(TSN.fbroadcast_explicit, (Vector{Int},))
    sig, body = children(tsn)
    @test body.typ === Float64
    cnode = child(body, 2)
    cnodef = child(cnode, 1, 2, 1)
    @test kind(cnodef) == K"Identifier" && cnodef.val == :materialize
    @test cnode.typ === Vector{Float64}
    cnode = child(body, 2, 2)
    cnodef = child(cnode, 1, 2, 1)
    @test kind(cnodef) == K"Identifier" && cnodef.val == :broadcasted
    @test cnode.typ <: Broadcast.Broadcasted
    tsn = TypedSyntaxNode(TSN.fbroadcast2, (Vector{Int},))
    sig, body = children(tsn)
    node = child(body, 2)
    src = tsn.typedsource
    if isa(src.code[1], GlobalRef)
        @test kind(node) == K"dotcall" && node.typ === Vector{String}
    else
        # We aren't quite handling this properly yet
        @test_broken kind(node) == K"dotcall" && node.typ === Vector{String}
    end
    tsn = TypedSyntaxNode(TSN.bcast415, (TSN.B415, Float64))
    sig, body = children(tsn)
    @test child(body, 1).typ === Float64

    # Misc lowering
    tsn = TypedSyntaxNode(TSN.myunique, (AbstractRange,))
    sig, body = children(tsn)
    @test has_name_typ(child(body, 2), :r, AbstractRange)
    @test_broken has_name_typ(child(body, 3, 2), :r, AbstractRange)

    # kwfuncs
    st = """
    function avoidzero(x; avoid_zero=true)
        fx = float(x)
        return iszero(x) ? oftype(fx, NaN) : fx
    end
    """
    rootnode = JuliaSyntax.parsestmt(SyntaxNode, st; filename="TSN6.jl")
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
    @test child(body, 2, 1, 2).typ == Float64

    # default positional arguments
    tsn = TypedSyntaxNode(TSN.defaultarg, (Float32,))
    sig, body = children(tsn)
    @test has_name_typ(child(sig, 2), :x, Float32)
    @test has_name_notyp(child(sig, 3, 1), :y)
    # there is no argument 2 in tsn.typedsource
    tsn = TypedSyntaxNode(TSN.defaultarg, (Float32,Int))
    sig, body = children(tsn)
    @test has_name_typ(child(sig, 2), :x, Float32)
    nodearg = child(sig, 3)
    @test kind(nodearg) == K"="
    @test has_name_typ(child(nodearg, 1), :y, Int)
    # default position args that are types
    tsn = TypedSyntaxNode(TSN.hasdefaulttypearg, (Type{Float32},))
    sig, body = children(tsn)
    arg = child(sig, 1, 2, 1)
    @test kind(arg) == K"::" && arg.typ === Type{Float32}
    tsn = TypedSyntaxNode(TSN.hasdefaulttypearg, ())
    sig, body = children(tsn)
    arg = child(sig, 1, 2, 1)
    @test_broken kind(arg) == K"::" && arg.typ === Type{Rational{Int}}  # maybe this shouldn't even be true

    # macros in function definition
    tsn = TypedSyntaxNode(TSN.mysin, (Int,))
    @test kind(tsn) == K"macrocall"
    sig, body = children(child(tsn, 2))
    @test has_name_typ(child(sig, 2, 1), :x, Int)
    @test has_name_typ(child(body, 1, 1), :xf, Float64)

    # generated functions
    # This mostly tests that we don't choke on assignments to variables whose names do not show up in the source
    tsn = TypedSyntaxNode(TSN.generated385, (Vector{Int},))
    @test kind(tsn) == K"macrocall"
    sig, body = children(child(tsn, 2))
    @test has_name_typ(child(sig, 2, 1), :dest, Vector{Int})

    # `for` loops
    tsn = TypedSyntaxNode(TSN.summer, (Vector{Float64},))
    @test tsn.typ == Union{Int,Float64}
    sig, body = children(tsn)
    @test has_name_typ(child(sig, 2), :list, Vector{Float64})
    @test has_name_typ(child(body, 1, 1), :s, Int)
    @test has_name_typ(child(body, 2, 1, 1), :x, Float64)
    node = child(body, 2, 2, 1)
    @test kind(node) == K"+="
    @test has_name_typ(child(node, 1), :s, Float64)   # if this line runs, the LHS now has type `Float64`
    @test has_name_typ(child(node, 2), :x, Float64)
    @test has_name_typ(child(body, 3, 1), :s, Union{Float64, Int})
    tsn = TypedSyntaxNode(TSN.summer_iterate, (Vector{Float64},))
    @test tsn.typ == Union{Int,Float64}
    sig, body = children(tsn)
    @test has_name_typ(child(body, 2, 1), :ret, Union{Nothing, Tuple{Float64, Int64}})
    @test has_name_typ(child(body, 3, 2, 1, 1, 1), :x, Float64)

    # `where`, unnamed arguments, and types-as-arguments
    tsn = TypedSyntaxNode(TSN.zerowhere, (Vector{Int16},))
    sig, body = children(tsn)
    @test child(sig, 1, 2).typ === Vector{Int16}
    @test body.typ === Int16
    @test has_name_typ(child(body, 2), :T, Type{Int16})
    # tsn = TypedSyntaxNode(TSN.vaparam, (Matrix{Float32}, (String, Bool)))    # fails on `which`
    m = @which TSN.vaparam(rand(3,3), ("hello", false))
    mi = first(specializations(m))
    tsn = TypedSyntaxNode(mi)
    sig, body = children(tsn)
    @test has_name_typ(child(sig, 1, 3, 1), :I, Tuple{String, Bool})
    tsn = TypedSyntaxNode(TSN.myplustv, (Char, UInt8))
    sig, body = children(TypedSyntax.get_function_def(tsn))
    @test has_name_typ(child(sig, 1, 2, 1), :x, Char)
    @test has_name_typ(child(sig, 1, 3, 1), :y, UInt8)
    tsn = TypedSyntaxNode(TSN.myplustv, (AbstractChar, UInt8))
    sig, body = children(TypedSyntax.get_function_def(tsn))
    @test has_name_typ(child(sig, 1, 2, 1), :x, AbstractChar)
    @test has_name_typ(child(sig, 1, 3, 1), :y, UInt8)
    tsn = TypedSyntaxNode(TSN.cb, (Vector{Int16}, Int))
    sig, body = children(tsn)
    @test has_name_typ(child(body, 2), :Bool, Type{Bool})
    tsn = TypedSyntaxNode(TSN.unnamedargs, (Type{Matrix{Float32}}, Type{Int}))
    sig, body = children(tsn)
    m = @which TSN.unnamedargs(Matrix{Float32}, Int, Int)
    fbody = Base.bodyfunction(m)
    @test child(sig, 1, 2).typ === Type{Matrix{Float32}}
    @test child(sig, 1, 3).typ === Type{Int}
    m = @which TSN.unnamedargs(Matrix{Float32}, Int; a="hello")
    mi = nothing
    for _mi in specializations(m)
        _mi === nothing && continue
        nt = _mi.specTypes.parameters[2]
        nt <: NamedTuple || continue
        if nt == NamedTuple{(:a,), Tuple{String}}
            mi = _mi
            break
        end
    end
    tsn = TypedSyntaxNode(mi)
    sig, body = children(tsn)
    @test child(sig, 1, 2).typ === Type{Matrix{Float32}}
    @test child(sig, 1, 3).typ === Type{Int}
    @test has_name_notyp(child(sig, 1, 4, 1), :c)
    @test has_name_typ(child(sig, 1, 5, 1, 1), :a, String)
    m = @which TSN.unnamedargs(Matrix{Float32}, Int, :c; a="hello")
    mi = nothing
    for _mi in specializations(m)
        _mi === nothing && continue
        if any(==(Symbol), _mi.specTypes.parameters)
            mi = _mi
            break
        end
    end
    tsn = TypedSyntaxNode(mi)
    sig, body = children(tsn)
    @test child(sig, 1, 2).typ === Type{Matrix{Float32}}
    @test child(sig, 1, 3).typ === Type{Int}
    @test child(sig, 1, 4, 1).typ === Symbol
    @test child(sig, 1, 5, 1, 1).typ === String
    mbody = only(methods(fbody))
    mi = nothing
    for _mi in specializations(mbody)
        _mi === nothing && continue
        if any(==(Symbol), _mi.specTypes.parameters)
            mi = _mi
            break
        end
    end
    tsn = TypedSyntaxNode(mi)
    sig, body = children(tsn)
    @test child(sig, 1, 2).typ === Type{Matrix{Float32}}
    @test child(sig, 1, 3).typ === Type{Int}
    @test child(sig, 1, 4, 1).typ === Symbol
    @test child(sig, 1, 5, 1, 1).typ === String
    tsn = TypedSyntaxNode(TSN.unnamedargs2, (Type{Matrix}, Symbol))
    sig, body = children(tsn)
    @test child(sig, 2).typ === Type{Matrix}
    @test child(sig, 3, 1).typ === Symbol
    @test has_name_notyp(child(sig, 4, 1, 1, 1), :padding)

    # Values as static_parameters
    tsn = TypedSyntaxNode(TSN.val, (Val{4},))
    sig, body = children(tsn)
    @test TypedSyntax.child(body, 1, 1, 1).typ == Core.Const(4)

    # varargs
    tsn = TypedSyntaxNode(TSN.likevect, (Int, Int))
    sig, body = children(tsn)
    nodeva = child(sig, 1, 2)
    @test kind(nodeva) == K"..."
    @test has_name_typ(child(nodeva, 1, 1), :X, Tuple{Int,Int})
    tsn = TypedSyntaxNode(TSN.cbva, (Matrix{Float32}, Int, Int))
    sig, body = children(tsn)
    @test body.typ === Bool
    @test has_name_typ(child(body, 2), :Bool, Type{Bool})
    @test has_name_typ(child(body, 3), :a, Matrix{Float32})
    nodeva = child(body, 4)
    @test kind(nodeva) == K"..."
    @test has_name_typ(child(nodeva, 1), :i, Tuple{Int,Int})
    tsn = TypedSyntaxNode(TSN.splats, (Tuple{Int,Int}, Tuple{Int}))
    sig, body = children(tsn)
    @test body.typ === Vector{Int}
    @test has_name_typ(child(body, 2, 1), :x, Tuple{Int,Int})
    @test has_name_typ(child(body, 3, 1), :y, Tuple{Int})
    m = @which TSN.anykwargs(; cat=1, dog=2)
    mi = first(specializations(m))
    tsn = TypedSyntaxNode(mi)
    src = tsn.typedsource
    @test Symbol("kwargs...") ∈ src.slotnames
    sig, body = children(tsn)
    @test child(body, 2, 1).typ <: Base.Iterators.Pairs

    # quoted symbols that could be confused for function definition
    tsn = TypedSyntaxNode(TSN.isexpreq, (Expr,))
    sig, body = children(tsn)
    @test has_name_typ(child(sig, 2, 1), :ex, Expr)

    # Unused statements
    tsn = TypedSyntaxNode(TSN.mycheckbounds, (Vector{Int}, Int))
    @test tsn.typ === Nothing
    sig, body = children(tsn)
    errnode = child(body, 1, 2)
    errf = child(errnode, 1)
    @test errnode.typ === nothing && errf.typ === typeof(Base.throw_boundserror)
    retnode = child(body, 2)
    @test kind(retnode) == K"return"
    @test retnode.typ === nothing || retnode.typ === Nothing

    # Globals & scoped assignment
    tsn = TypedSyntaxNode(TSN.setglobal, (Char,))
    # Agnostic about whether it's good to tag the type of `myglobal`, but at least `val` should be tagged
    sig, body = children(tsn)
    @test has_name_typ(child(body, 1, 1, 2), :val, Char)

    # DataTypes
    tsn = TypedSyntaxNode(TSN.myoftype, (Float64, Int))
    sig, body = children(tsn)
    node = child(body, 1)
    @test node.typ === Type{Float64}
    tsn = TypedSyntaxNode(TSN.DefaultArray{Float32}, (Vector{Int}, Int))
    sig, body = children(tsn)
    @test kind(sig) == K"where"
    @test kind(child(sig, 1)) == K"call"
    f = child(sig, 1, 1)
    @test kind(f) == K"curly" && f.typ === Type{TSN.DefaultArray{Float32}}

    # Field access & a more complex example
    tsn = TypedSyntaxNode(Base.getindex, (TSN.DefaultArray{Float64,2,Matrix{Float64}}, Int, Int))
    @test tsn.typ === Float64
    sig, body = children(tsn)
    @test kind(body) == K"?"
    @test child(body, 1).typ === Bool
    nodeidx = child(body, 2)
    @test nodeidx.typ === Float64
    @test child(nodeidx, 1).typ === Matrix{Float64}
    default = child(body, 3)
    @test default.typ === Float64
    @test child(default, 1).typ === TSN.DefaultArray{Float64,2,Matrix{Float64}}

    # global
    tsn = TypedSyntaxNode(TSN.in_let, (Int,))
    sig, body = children(child(tsn, 1))
    @test has_name_typ(child(sig, 2), :x, Int)
    @test_broken body.typ == Int

    # Construction from MethodInstance
    src, rt = TypedSyntax.getsrc(TSN.myoftype, (Float64, Int))
    tsn = TypedSyntaxNode(src.parent)
    sig, body = children(tsn)
    node = child(body, 1)
    @test node.typ === Type{Float64}

    # UnionAll in signature (issue #409)
    @static if VERSION ≥ v"1.9-"
        tsn = TypedSyntaxNode(Core.kwcall, (NamedTuple, typeof(issorted), Vector{Int}))
        sig, body = children(tsn)
        @test has_name_typ(child(sig, 2), :itr, Vector{Int})
    end

    # Empty `return` (issue #458)
    tsn = TypedSyntaxNode(TSN.f458, ())
    @test tsn.typ === Nothing
    sig, body = children(tsn)
    @test kind(body) == K"return"
    @test body.typ === Nothing

    # Counting arguments (needed for Cthulhu)
    # issue #397
    tsn = TypedSyntaxNode(TSN.f397, (typeof(view([1,2,3], 1:2)),))
    @test TypedSyntax.num_positional_args(tsn) == 2 # the function is arg1, x is arg2
    # issue #426
    tsn = TypedSyntaxNode(getindex, (typeof(TSN.T426), Type{Year},))
    @test TypedSyntax.num_positional_args(tsn) == 3
    # issue #433
    tsn = TypedSyntaxNode(TSN.withrt, (IO,))
    @test TypedSyntax.num_positional_args(tsn) == 2

    # Display
    tsn = TypedSyntaxNode(TSN.mysin, (Int,))
    str = sprint(tsn; context=:color=>false) do io, obj
        printstyled(io, obj; hide_type_stable=false)
    end
    # There are several potential valid ways to print the signature.
    # Here we allow them all (some tests below may not be as flexible).
    # Arguably the first seems best, as it has the inferred type
    # bound more tightly to the object than the type-assertion in the signature,
    # and is valid syntax that is a bit more sparing in its use of ().
    @test occursin("function (\$f)(x::Int64::Real)::Float64", str) ||
          occursin("function (\$f)((x::Int64)::Real)::Float64", str) ||
          occursin("function (\$f)(x::Real::Int64)::Float64", str) ||
          occursin("function (\$f)((x::Real)::Int64)::Float64", str)
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
    tsn = TypedSyntaxNode(TSN.summer, (Vector{Float64},))
    str = sprint(tsn; context=:color=>false) do io, obj
        printstyled(io, obj; hide_type_stable=false)
    end
    @test   occursin("s::$Int = 0::$Int", str)
    @test !occursin("(s::$Int = 0::$Int)", str)
    @test occursin("(s::Float64 += x::Float64)::Float64", str)
    tsn = TypedSyntaxNode(TSN.zerowhere, (Vector{Int16},))
    str = sprint(tsn; context=:color=>true) do io, obj
        printstyled(io, obj; iswarn=true, hide_type_stable=false)
    end
    @test occursin("AbstractArray{T})\e[36m::Vector{Int16}\e[39m", str)
    @test occursin("where T<:Real)\e[36m::Int16\e[39m", str)
    str = sprint(tsn; context=:color=>false) do io, obj
        printstyled(io, obj; hide_type_stable=false)
    end
    # One could either have `(::AbstractArray{T})` or `::(AbstractArray{T})`
    # The latter is more consistent with how we want `-x` to print.
    @test occursin("(zerowhere(::(AbstractArray{T})::Vector{Int16}) where T<:Real)::Int16", str)
    @test occursin("zero(T::Type{Int16})", str)
    tsn = TypedSyntaxNode(TSN.add2, (Vector{Float32},))
    str = sprint(tsn; context=:color=>true) do io, obj
        printstyled(io, obj; iswarn=true, hide_type_stable=false)
    end
    @test occursin("[1]\e[36m::Float32\e[39m", str)
    @test occursin("[2]\e[36m::Float32\e[39m", str)
    tsn = TypedSyntaxNode(TSN.simplef, (Int, Float64))
    str = sprint(tsn; context=:color=>false) do io, obj
        printstyled(io, obj; hide_type_stable=false)
    end
    @test str === """
        5 function simplef(a::Int64, b::Float64)::Float64
        6     z::Int64 = (a::Int64 * a::Int64)::Int64
        7     return (z::Int64 + b::Float64)::Float64
        8 end"""
    tsn = TypedSyntaxNode(TSN.myabs, (Float64,))
    str = sprint(tsn; context=:color=>false) do io, obj
        printstyled(io, obj; hide_type_stable=false)
    end
    @test occursin("-(x::Float64)::Float64", str)

    # issue #413
    @test TypedSyntax.is_small_union_or_tunion(Union{})
    @test TypedSyntax.is_small_union_or_tunion(Union{Tuple{}, Tuple{Int64}})

    # artificial trigger for #414
    tsn = TypedSyntaxNode(TSN.unnamedargs, (Type{Matrix{Float32}}, Type{Int}))
    sig, body = children(tsn)
    @test isa(TypedSyntax.map_signature!(sig, fill(Symbol(""), 3), Any[Any, Core.Const(2), Any]), TypedSyntaxNode)

    # issue #435
    tsnc = copy(tsn)
    @test isa(tsnc, TypedSyntaxNode)
end

if parse(Bool, get(ENV, "CI", "false"))
    include("exhaustive.jl")
end

module VSCodeServer
    struct InlineDisplay
        is_repl::Bool
    end
    const INLAY_HINTS_ENABLED = Ref(true)

    function Base.display(d::InlineDisplay, x)
        return nothing
    end
end
module TestVSCodeExt # stops modules defined in test files from overwriting stuff from previous test
using Test, ..VSCodeServer

@testset "VSCode TypedSyntax.jl" begin
    @testset "test_vscode.jl" begin
        include("test_vscode.jl")
    end
end
end