# module test_Cthulhu

using Test, Cthulhu, StaticArrays, Random, Accessors
using Core: Const

global _Cthulhu::Module = Cthulhu.CTHULHU_MODULE[]
using ._Cthulhu: CC, DefaultProvider, CthulhuConfig
global CONFIG::CthulhuConfig = _Cthulhu.CONFIG


include("setup.jl")
include("irutils.jl")

# NOTE: From Julia version `v"1.12.0-DEV.1581"` onwards, Cthulhu uses the Compiler.jl stdlib.
# Therefore, for queries on its data structures, use the utilities from `Compiler === Cthulhu.CC`
# instead of those from `Base.Compiler === Core.Compiler`.

@testset "ambiguity check" begin
    @test isempty(detect_ambiguities(Cthulhu))
end

function testf_simple()
    T = rand() > 0.5 ? Int64 : Float64
    sum(rand(T, 100))
end

function empty_func(::Bool) end

pure_concrete_eval() = exp((1, 1)[1])

anykwargs(a; kwargs...) = println(a, " keyword args: ", kwargs...)
hasdefaultargs(a, b=2) = a + b

function first_specialization(m::Method)
    specs = m.specializations
    if specs isa Core.MethodInstance
        return specs
    elseif specs isa Core.SimpleVector
        return something(specs...)::Core.MethodInstance
    else
        throw((m, specs))
    end
end

@testset "Callsites" begin
    callsites = find_callsites_by_ftt(testf_simple)
    @test length(callsites) >= 4

    callsites = find_callsites_by_ftt(testf_simple; optimize=false)
    @test length(callsites) == 4

    callsites = find_callsites_by_ftt(empty_func, Tuple{Bool}; optimize=true)
    @test isempty(callsites)

    # handle pure
    callsites = find_callsites_by_ftt(pure_concrete_eval; optimize=false)
    @test occursin("::Const((1, 1))", string(callsites[1]))
    @test occursin(r"< (constprop|concrete eval) > getindex\(::.*Const.*,::.*Const\(1\)\)::.*Const\(1\)", string(callsites[1]))

    callsites = @eval find_callsites_by_ftt(; optimize=false) do
        length($(QuoteNode(Core.svec(0,1,2))))
    end
    @test occursin(r"< (pure|concrete eval) >", string(callsites[1]))

    # Callsite handling in source-view mode: for kwarg functions, strip the body, and use "typed" callsites
    for m in (@which(anykwargs("animals")), @which(anykwargs("animals"; cat=1, dog=2)))
        mi = first_specialization(m)
        src, rt = Cthulhu.TypedSyntax.code_typed1_tsn(mi)
        tsn, mappings = Cthulhu.get_typed_sourcetext(mi, src, rt; warn=false)
        str = sprint(printstyled, tsn)
        @test occursin("anykwargs", str) && occursin("kwargs...", str) && !occursin("println", str)
        @test isempty(mappings)
    end
    # Likewise for methods that fill in default positional arguments
    m = @which hasdefaultargs(1)
    mi = first_specialization(m)
    src, rt = Cthulhu.TypedSyntax.code_typed1_tsn(mi)
    tsn, mappings = Cthulhu.get_typed_sourcetext(mi, src, rt; warn=false)
    str = sprint(printstyled, tsn)
    @test occursin("hasdefaultargs(a, b=2)", str)
    @test !occursin("a + b", str)
    @test isempty(mappings)
    m = @which hasdefaultargs(1, 5)
    mi = first_specialization(m)
    src, rt = Cthulhu.TypedSyntax.code_typed1_tsn(mi)
    tsn, mappings = Cthulhu.get_typed_sourcetext(mi, src, rt; warn=false)
    str = sprint(printstyled, tsn)
    @test occursin("hasdefaultargs(a, b=2)", str)
    @test occursin("a + b", str)
    @test !isempty(mappings)
end

@testset "Expr heads" begin
    # Check that the Expr head (:invoke or :call) is preserved
    @noinline twice(x::Real) = 2x
    calltwice(c) = twice(c[1])

    callsites = find_callsites_by_ftt(calltwice, Tuple{Vector{Float64}})
    @test any(callsites) do callsite
        callsite.head === :invoke || return false
        io = IOBuffer()
        print(io, callsite)
        return occursin("invoke twice(::Float64)::Float64", String(take!(io)))
    end
    @test any(callsites) do callsite
        callsite.head === :invoke || return false
        io = IOBuffer()
        print(io, callsite)
        return occursin("invoke throw_boundserror", String(take!(io)))
    end

    callsites = find_callsites_by_ftt(calltwice, Tuple{Vector{AbstractFloat}})
    @test any(callsites) do callsite
        callsite.head === :call || return false
        io = IOBuffer()
        print(io, callsite)
        return occursin("call twice(::AbstractFloat)", String(take!(io)))
    end
    @test any(callsites) do callsite
        callsite.head === :invoke || return false
        io = IOBuffer()
        print(io, callsite)
        return occursin("invoke throw_boundserror", String(take!(io)))
    end

    # Note the failure of `callinfo` to properly handle specialization
    @test_broken Cthulhu.callinfo(Tuple{typeof(twice), AbstractFloat}, AbstractFloat) isa Cthulhu.MultiCallInfo
end

# https://github.com/JuliaDebug/Cthulhu.jl/issues/196
let
    callsites = @find_callsites_by_ftt tryparse(VersionNumber, "v2.1.3")
    @test !isempty(callsites)
    callsites = @find_callsites_by_ftt optimize=false tryparse(VersionNumber, "v2.1.3")
    @test !isempty(callsites)
end

# Check that we see callsites that are the rhs of assignments
@noinline bar_callsite_assign() = nothing
function foo_callsite_assign()
    x = bar_callsite_assign()
    x
end
let callsites = find_callsites_by_ftt(foo_callsite_assign, Tuple{}; optimize=false)
    @test length(callsites) == 1
end

@eval function call_rt()
    S = $(CC.return_type)(+, Tuple{Int, Int})
end
let callsites = find_callsites_by_ftt(call_rt, Tuple{}; optimize=false)
    @test length(callsites) == 1
end

function call_by_apply(args...)
    identity(args...)
end
let callsites = find_callsites_by_ftt(call_by_apply, Tuple{Tuple{Int}}; optimize=false)
    @test length(callsites) == 1
end

Base.@propagate_inbounds _boundscheck_dce(x) = @boundscheck error()
boundscheck_dce_inbounds(x) = @inbounds _boundscheck_dce(x)
boundscheck_dce(x) = _boundscheck_dce(x)

@testset "DCE & boundscheck" begin
    # no boundscheck elimination on Julia-level compilation
    for f in (boundscheck_dce_inbounds, boundscheck_dce)
        (_, _, _, result) = cthulhu_info(f, Tuple{Vector{Float64}})
        @test count(stmt -> isexpr(stmt, :boundscheck), result.ir.stmts.stmt) == 1
    end
end

# Something with many methods, but enough to be under the match limit
g_matches(a::Int, b::Int) = a+b
g_matches(a::Float64, b::Float64) = a+b
f_matches(a, b) = g_matches(a, b)
let callsites = find_callsites_by_ftt(f_matches, Tuple{Any, Any}; optimize=false)
    @test length(callsites) == 1
    callinfo = callsites[1].info
    @test callinfo isa Cthulhu.MultiCallInfo
    @test Cthulhu.get_effects(callinfo) |> CC.is_foldable
    io = IOBuffer()
    Cthulhu.show_callinfo(io, callinfo)
    @test occursin(r"→ g_matches\(::Any, ?::Any\)::Union{Float64, ?Int\d+}", String(take!(io)))
end

uncached_call1(a) = uncached_call2(a)
uncached_call2(a) = somecode::Bool ? uncached_call3(a) : a
uncached_call3(a) = uncached_call1(Type{a})

@testset "ConstPropCallInfo" begin
    @testset "union-split constant-prop'ed callsites" begin
        # constant prop' on all the splits
        let callsites = (@eval Module() begin
                mutable struct F32
                    val::Float32
                    _v::Int
                end
                mutable struct F64
                    val::Float64
                    _v::Int
                end

                $find_callsites_by_ftt((Union{F32,F64},); optimize = false) do f
                    f.val
                end
            end)
            @test length(callsites) == 1
            callinfo = callsites[1].info
            @test isa(callinfo, Cthulhu.MultiCallInfo)
            callinfos = callinfo.callinfos
            @test length(callinfos) == 2
            @test all(ci->isa(ci, Cthulhu.ConstPropCallInfo), callinfos)
            io = IOBuffer()
            Cthulhu.show_callinfo(io, callinfos[1])
            @test startswith(String(take!(io)), "getproperty")
            io = IOBuffer()
            print(io, callsites[1])
            @test occursin("= call → getproperty", String(take!(io)))
        end

        # const and non-const splits
        let callsites = find_callsites_by_ftt((Bool,Vector{Any},); optimize=false) do cond, t
                if cond
                    t = (1, nothing)
                end
                t[1]
            end
            @test length(callsites) == 1 # getindex(::Union{Vector{Any}, Const(tuple(1,nothing))}, ::Const(1))
            callinfo = callsites[1].info
            @test isa(callinfo, Cthulhu.MultiCallInfo)
            callinfos = callinfo.callinfos
            @test length(callinfos) == 2
            @test count(ci->isa(ci, Cthulhu.EdgeCallInfo), callinfos) == 1 # getindex(::Vector{Any}, ::Const(1))
            @test count(ci->isa(ci, Cthulhu.ConstPropCallInfo) || isa(ci, Cthulhu.SemiConcreteCallInfo), callinfos) == 1 # getindex(::Const(tuple(1,nothing)), ::Const(1))
        end

        let callsites = (@eval Module() begin
                mutable struct F32
                    val::Float32
                    _v::Int
                end

                $find_callsites_by_ftt((F32,); optimize = false) do f
                    f.val
                end
            end)
            io = IOBuffer()
            print(io, callsites[1])
            @test occursin("= < constprop > getproperty(", String(take!(io)))
        end
    end
end

Base.@assume_effects :terminates_locally function issue41694(x)
    res = 1
    1 < x < 20 || throw("bad")
    while x > 1
        res *= x
        x -= 1
    end
    return res
end
@testset "ConstResult" begin
    # constant prop' on all the splits
    callsites = find_callsites_by_ftt(() -> issue41694(12); optimize = false)
    callinfo = only(callsites).info
    @test isa(callinfo, Cthulhu.ConcreteCallInfo)
    @test Cthulhu.get_rt(callinfo) == Const(factorial(12))
    @test Cthulhu.get_effects(callinfo) |> CC.is_foldable
    io = IOBuffer()
    print(io, only(callsites))
    @test occursin("= < concrete eval > issue41694(::Const(12))", String(take!(io)))
end

let # check the performance benefit of semi concrete evaluation
    param = 1000
    ex = Expr(:block)
    var = gensym()
    push!(ex.args, :($var = x))
    for _ = 1:param
        newvar = gensym()
        push!(ex.args, :($newvar = sin($var)))
        var = newvar
    end
    @eval global Base.@constprop :aggressive Base.@assume_effects :nothrow function semi_concrete_eval(x::Int, _::Int)
        out = $ex
        out
    end
end
@testset "SemiConcreteResult" begin
    # constant prop' on all the splits
    callsites = find_callsites_by_ftt(x -> semi_concrete_eval(42, x), (Int,); optimize = false)
    callinfo = only(callsites).info
    @test isa(callinfo, Cthulhu.SemiConcreteCallInfo)
    @test Cthulhu.get_rt(callinfo) == Const(semi_concrete_eval(42, 0))
    # @test Cthulhu.get_effects(callinfo) |> CC.is_semiconcrete_eligible
    io = IOBuffer()
    print(io, only(callsites))
    @test occursin("= < semi-concrete eval > semi_concrete_eval(::Const(42),::$Int)", String(take!(io)))
end

function bar346(x::ComplexF64)
    x = ComplexF64(x.re, 1.0)
    return sin(x.im)
end
@testset "issue #346" begin
    callsites = find_callsites_by_ftt(bar346, Tuple{ComplexF64}; optimize=false)
    @test isa(callsites[1].info, Cthulhu.SemiConcreteCallInfo)
    @test occursin("= < semi-concrete eval > getproperty(::ComplexF64,::Const(:re))::Float64", string(callsites[1]))
    @test Cthulhu.get_rt(callsites[end].info) == Const(sin(1.0))
end

struct SingletonPureCallable{N} end

@testset "PureCallInfo" begin
    c1 = Cthulhu.PureCallInfo(Any[typeof(sin), Float64], Float64)
    s = sprint(Cthulhu.show_callinfo, c1)
    @test occursin("sin(::Float64)::Float64", s)

    c2 =  Cthulhu.PureCallInfo(Any[SingletonPureCallable{1}, Float64], Float64)
    s = sprint(Cthulhu.show_callinfo, c2)
    @test occursin("SingletonPureCallable{1}()(::Float64)::Float64", s)


    @test Cthulhu.get_effects(c1) |> CC.is_foldable_nothrow
    @test Cthulhu.get_effects(c2) |> CC.is_foldable_nothrow
end

@testset "ReturnTypeCallInfo" begin
    only_ints(::Integer) = 1

    callsites = find_callsites_by_ftt(; optimize=false) do
        t1 = CC.return_type(only_ints, Tuple{Int})     # successful `return_type`
        t2 = CC.return_type(only_ints, Tuple{Float64}) # failed `return_type`
        t1, t2
    end
    # We have the function resolved as `getproperty(Compiler, :return_type)` first.
    @test length(callsites) == 4
    extract_callsite(i) = callsites[2i]
    callinfo1 = extract_callsite(1).info
    @test callinfo1 isa Cthulhu.ReturnTypeCallInfo
    @test callinfo1.vmi isa Cthulhu.EdgeCallInfo
    io = IOBuffer()
    Cthulhu.show_callinfo(io, callinfo1)
    @test String(take!(io)) == "only_ints(::$Int)::$Int"
    io = IOBuffer()
    print(io, extract_callsite(1))
    @test occursin("return_type < only_ints(::$Int)::$Int >", String(take!(io)))

    callinfo2 = extract_callsite(2).info
    @test callinfo2 isa Cthulhu.ReturnTypeCallInfo
    @test callinfo2.vmi isa Cthulhu.FailedCallInfo
    io = IOBuffer()
    Cthulhu.show_callinfo(io, callinfo2)
    @test String(take!(io)) == "only_ints(::Float64)::Union{}"
    io = IOBuffer()
    print(io, extract_callsite(2))
    @test occursin("return_type < only_ints(::Float64)::Union{} >", String(take!(io)))

    @test Cthulhu.get_effects(callinfo1) |> CC.is_foldable_nothrow
    @test Cthulhu.get_effects(callinfo2) |> CC.is_foldable_nothrow
end

@testset "OCCallInfo" begin
    callsites = find_callsites_by_ftt((Int,Int,); optimize=false) do a, b
        oc = Base.Experimental.@opaque b -> sin(a) + cos(b)
        oc(b)
    end
    @test length(callsites) == 1
    callinfo = only(callsites).info
    @test callinfo isa Cthulhu.OCCallInfo
    @test Cthulhu.get_effects(callinfo) |> !CC.is_foldable_nothrow
    # TODO not sure what these effects are (and neither is Base.infer_effects yet)
    @test callinfo.ci.rt === Base.return_types((Int,Int)) do a, b
        sin(a) + cos(b)
    end |> only === Float64

    buf = IOBuffer()
    Cthulhu.show_callinfo(buf, callinfo.ci)
    s = "opaque closure(::$Int)::$Float64"
    @test String(take!(buf)) == s
    print(buf, only(callsites))
    @test occursin("< opaque closure call > $s", String(take!(buf)))

    # const-prop'ed OC callsite
    callsites = find_callsites_by_ftt((Int,); optimize=false) do a
        oc = Base.Experimental.@opaque Base.@constprop :aggressive b -> sin(b)
        oc(42)
    end

    @test length(callsites) == 1
    callinfo = only(callsites).info
    @test callinfo isa Cthulhu.OCCallInfo
    inner = callinfo.ci
    @test inner isa Cthulhu.ConstPropCallInfo || inner isa Cthulhu.SemiConcreteCallInfo

    buf = IOBuffer()
    Cthulhu.show_callinfo(buf, callinfo.ci)
    s = "opaque closure(::$(Const(42)))::$(Const(sin(42)))"
    @test String(take!(buf)) == s
    print(buf, only(callsites))
    @test occursin("< opaque closure call > $s", String(take!(buf)))
end

# tasks
ftask() = @sync @async show(io, "Hello")
let callsites = find_callsites_by_ftt(ftask, Tuple{})
    task_callsites = filter(c->c.info isa Cthulhu.TaskCallInfo, callsites)
    @test !isempty(task_callsites)
    @test filter(c -> c.info.ci isa Cthulhu.FailedCallInfo, task_callsites) == []
    io = IOBuffer()
    show(io, first(task_callsites))
    @test occursin("= task < #", String(take!(io)))
end

invoke_call(::Integer) = :Integer
invoke_call(::Int)     = :Int
invoke_constcall(a::Any,    c::Bool) = c ? Any : :any
invoke_constcall(a::Number, c::Bool) = c ? Number : :number

@testset "InvokeCallInfo" begin
    callsites = find_callsites_by_ftt((Int,); optimize=false) do n
        Base.@invoke invoke_call(n::Integer)
    end
    callsite = only(callsites)
    info = callsite.info
    @test isa(info, Cthulhu.InvokeCallInfo)
    @test Cthulhu.get_effects(info) |> CC.is_foldable_nothrow
    rt = CC.Const(:Integer)
    @test info.ci.rt === rt
    buf = IOBuffer()
    show(buf, callsite)
    @test occursin("= invoke < invoke_call(::$Int)::$rt >", String(take!(buf)))

    callsites = find_callsites_by_ftt((Int,); optimize=false) do n
        Base.@invoke invoke_call(n::Int)
    end
    callsite = only(callsites)
    info = callsite.info
    @test isa(info, Cthulhu.InvokeCallInfo)
    @test Cthulhu.get_effects(info) |> CC.is_foldable_nothrow
    @test info.ci.rt === CC.Const(:Int)

    # const prop' / semi-concrete callsite
    callsites = find_callsites_by_ftt((Any,); optimize=false) do a
        Base.@invoke invoke_constcall(a::Any, true::Bool)
    end
    callsite = only(callsites)
    info = callsite.info
    @test isa(info, Cthulhu.InvokeCallInfo)
    @test Cthulhu.get_effects(info) |> CC.is_foldable_nothrow
    inner = info.ci
    rt = Const(Any)
    @test Cthulhu.get_rt(info) === rt
    buf = IOBuffer()
    show(buf, callsite)
    @test isa(inner, Cthulhu.SemiConcreteCallInfo)
    @test occursin("= invoke < invoke_constcall(::Any,::$(Const(true)))::$rt", String(take!(buf)))
end

##
# Union{f, g}
##
callf(f::F, x) where F = f(x)
let callsites = find_callsites_by_ftt(callf, Tuple{Union{typeof(sin), typeof(cos)}, Float64})
    @test !isempty(callsites)
    if length(callsites) == 1
        @test first(callsites).info isa Cthulhu.MultiCallInfo
        callinfos = first(callsites).info.callinfos
        @test !isempty(callinfos)
        mis = map(Cthulhu.get_mi, callinfos)
        @test any(mi->mi.def.name === :cos, mis)
        @test any(mi->mi.def.name === :sin, mis)
    else
        @test all(cs->cs.info isa Union{Cthulhu.EdgeCallInfo,Cthulhu.MultiCallInfo}, callsites)
    end
end

function toggler(toggle)
    if toggle
        g = sin
    else
        g = cos
    end
    g(0.0)
end
let callsites = find_callsites_by_ftt(toggler, Tuple{Bool})
    @test !isempty(callsites)
    if length(callsites) == 1
        @test first(callsites).info isa Cthulhu.MultiCallInfo
        callinfos = first(callsites).info.callinfos
        @test !isempty(callinfos)
        defs = map(get_method, callinfos)
        @test any(def -> def.name === :cos, defs)
        @test any(def -> def.name === :sin, defs)
    else
        @test all(cs->cs.info isa Union{Cthulhu.EdgeCallInfo,Cthulhu.MultiCallInfo}, callsites)
    end
end

@testset "Varargs and printing" begin
    @test Cthulhu.headstring(Float32) == "Float32"
    @test Cthulhu.headstring(Vararg{Float32}) == "Float32"
    @test Cthulhu.headstring(Union{Char,Float32}) == "Union{Char, Float32}"
    @test Cthulhu.headstring(Union{}) == "Union{}"
    @test Cthulhu.headstring(Vector{Int}) == "Array"
    @test Cthulhu.headstring(Vector{T} where T) == "Array"
    @test Cthulhu.headstring(TypeVar(:F)) == "F"

    function checklim(f, n, strchar::Union{AbstractString,AbstractChar})
        io = IOBuffer()
        iolim = Cthulhu.TextWidthLimiter(io, n)
        print(iolim, strchar)
        return f(String(take!(iolim)))
    end
    @test checklim(str -> str == "α", 3, 'α')
    @test checklim(str -> str == "…", 1, 'a')
    @test checklim(str ->  occursin("…", str), 80, "abcd"^21)
    @test checklim(str -> !occursin("…", str), 80, "abcd"^19)

    function checklim(f, n, info::Cthulhu.CallInfo)
        io = IOBuffer()
        iolim = Cthulhu.TextWidthLimiter(io, n)
        Cthulhu.show_callinfo(iolim, info)
        return f(String(take!(iolim)))
    end
    function fsplat(::Type{Int}, a...)
        z = zero(Int)
        for v in a
            z += v
        end
        return z
    end
    gsplat1(T::Type, a...) = fsplat(T, a...)   # does not force specialization
    hsplat1(A) = gsplat1(eltype(A), A...)
    for (Atype, haslen) in ((Tuple{Tuple{Int,Int,Int}}, true),
                            (Tuple{Vector{Int}}, false),
                            (Tuple{SVector{3,Int}}, true))
        let callsites = find_callsites_by_ftt(hsplat1, Atype; optimize=false)
            @test !isempty(callsites)
            cs = callsites[end]
            @test cs isa Cthulhu.Callsite
            mi = get_mi(cs)
            @test mi.specTypes.parameters[end] === (haslen ? Int : Vararg{Int})
            @test checklim(str -> occursin("...", str) != haslen, 80, cs.info)
        end
    end
    callsites = find_callsites_by_ftt(hsplat1, Tuple{NTuple{10,Int}}; optimize=false)
    cs = callsites[end]
    @test checklim(str -> occursin("gsplat1(…,…,…,…,…,…,…,…,…,…,…)::Int", str), 80, cs.info)
    callsites = find_callsites_by_ftt(hsplat1, Tuple{NTuple{50,Int}}; optimize=false)
    cs = callsites[end]
    @test checklim(str -> occursin("gsplat1(…)::Int", str), 80, cs.info)

    # foo(x::Vector{Vector{Vector{Vector{Char}}}}) = -1
    foo(x::Vector) = -1
    bar() = foo([[[['c']]]])
    callsites = find_callsites_by_ftt(bar, Tuple{}; optimize=false)
    cs = callsites[end]
    checklim(80, cs.info) do str
        @test !occursin("Array{…}", str)
        @test occursin("::Const(-1)", str)
    end
    checklim(55, cs.info) do str
        @test !occursin("Array{…}", str)
        @test !occursin("::Const(-1)", str)
    end
    checklim(40, cs.info) do str
        @test occursin("Array{…}", str)
        @test occursin("::Const(-1)", str)
    end
    checklim(25, cs.info) do str
        @test occursin("Array{…}", str)
        @test !occursin("::Const(-1)", str)
    end
    checklim(8, cs.info) do str
        @test str == "foo(…)"
    end
    checklim(4, cs.info) do str
        @test str == "…"
    end
end

@testset "MaybeUndef" begin
    function maybeundef(b::Bool)
        b || @goto final_step
        str = randstring(8)
        @label final_step
        return str*"end"
    end
    @test isa(maybeundef(true), String)
    @test_throws UndefVarError maybeundef(false)
    cs = find_callsites_by_ftt(maybeundef, Tuple{Bool})[end]
    @test cs.head === :invoke
    if !isdefined(Base, :_string)
        @test get_method(cs) == which(string, (String,String))
    else
        @test get_method(cs) ∈ [which(string, (String,String)), only(methods(Base._string))]
    end
end

@testset "warntype variables" begin
    provider, mi, ci, result = cthulhu_info(identity, (Any,); optimize = false)
    state = CthulhuState(provider; mi, ci)
    buffer = IOBuffer()
    io = IOContext(buffer, :color => true)
    Cthulhu.cthulhu_warntype(io, provider, state, result)
    str = String(take!(buffer))
    @test occursin("x\e[91m\e[1m::Any\e[22m\e[39m", str)
end

@testset "Limit printing (issue #94)" begin
    m = Module()
    @eval m begin
        const x = collect(1:1000)
        f1() = x
        function f2()
            y = x
            return @noinline sum(y)
        end
    end
    function doprint(f)
        provider, mi, ci, result = cthulhu_info(f; optimize = false)
        config = @set CONFIG.view = :typed
        state = CthulhuState(provider; mi, ci, config)
        io = IOBuffer()
        Cthulhu.cthulhu_typed(io, provider, state, result)
        return String(take!(io))
    end
    @test occursin("invoke f1()::…\n", doprint(m.f1))
    str = doprint(m.f2)
    @test occursin("y::Const([1, 2, 3", str)
    @test !occursin("500,", str)
end

@testset "Issue #132" begin
    f132(w, dim) = [i == dim ? w[i]/2 : w[i]/1 for i in eachindex(w)]
    callsites = find_callsites_by_ftt(f132, (Vector{Int}, Int))
    @test !isempty(callsites) # just check that the above succeeded
end

# ## Functions for "backedges & treelist"
# # The printing changes when the functions are defined inside the testset
# fbackedge1() = 1
# fbackedge2(x) = x > 0 ? fbackedge1() : -fbackedge1()
# fst1(x) = backtrace()
# @inline fst2(x) = fst1(x)
# @noinline fst3(x) = fst2(x)
# @inline fst4(x) = fst3(x)
# fst5(x) = fst4(x)

# @testset "backedges and treelist" begin
#     @test fbackedge2(0.2) == 1
#     @test fbackedge2(-0.2) == -1
#     mi = first_specialization(@which(fbackedge1()))
#     root = Cthulhu.treelist(mi)
#     @test Cthulhu.count_open_leaves(root) == 2
#     @test root.data.callstr == "fbackedge1()"
#     @test root.children[1].data.callstr == " fbackedge2(::Float64)"

#     # issue #114
#     unspecva(@nospecialize(i::Int...)) = 1
#     @test unspecva(1, 2) == 1
#     mi = first_specialization(only(methods(unspecva)))
#     root = Cthulhu.treelist(mi)
#     @test occursin("Vararg", root.data.callstr)

#     # Test highlighting and other printing
#     mi = Cthulhu.get_specialization(:, Tuple{T, T} where T<:Integer)
#     root = Cthulhu.treelist(mi)
#     @test occursin("\e[31m::T\e[39m", root.data.callstr)
#     mi = Cthulhu.get_specialization(Vector{Int}, Tuple{typeof(undef), Int})
#     io = IOBuffer()
#     @test Cthulhu.callstring(io, mi) == "Vector{$Int}(::UndefInitializer, ::$Int)"
#     mi = Cthulhu.get_specialization(similar, Tuple{Type{Vector{T}}, Dims{1}} where T)
#     @test occursin(r"31m::Type", Cthulhu.callstring(io, mi))

#     # treelist for stacktraces
#     tree = Cthulhu.treelist(fst5(1.0))
#     @test match(r"fst1 at .*:\d+ => fst2 at .*:\d+ => fst3\(::Float64\) at .*:\d+", tree.data.callstr) !== nothing
#     @test length(tree.children) == 1
#     child = tree.children[1]
#     @test match(r" fst4 at .*:\d+ => fst5\(::Float64\) at .*:\d+", child.data.callstr) !== nothing

#     # issue #184
#     tree = Cthulhu.treelist(similar(fst5(1.0), 0))
#     @test isempty(tree.data.callstr)
#     @test isempty(Cthulhu.callstring(io, similar(stacktrace(fst5(1.0)), 0)))
#     @test Cthulhu.instance(similar(stacktrace(fst5(1.0)), 0)) === CC.Timings.ROOTmi
# end

@testset "ascend" begin
    # This tests only the non-interactive "look up the caller" portion
    local a::Int, b::Float64 = 3, 3.0 # avoid `callee(a)`/`callee(b)` to be constant-folded
    callee(x) = 2x
    function caller(x)
        val = 0.0
        val += callee(a); line1 = @__LINE__
        val += callee(b); line2 = @__LINE__
        val += callee(x); line3 = @__LINE__
        val = sum([val])     # FIXME: without this line, `lookup` fails because codeinst.inferred === nothing
        return val, line1, line2, line3
    end
    _, line1, line2, line3 = caller(7)
    provider = DefaultProvider()
    micaller = find_method_instance(provider, caller, (Int,))
    micallee_Int = find_method_instance(provider, callee, (Int,))
    micallee_Float64 = find_method_instance(provider, callee, (Float64,))
    info, lines = only(Cthulhu.find_caller_of(provider, micallee_Int, micaller))
    @test info == (:caller, Symbol(@__FILE__), 0) && lines == [line1, line3]
    info, lines = only(Cthulhu.find_caller_of(provider, micallee_Float64, micaller))
    @test info == (:caller, Symbol(@__FILE__), 0) && lines == [line2]

    M = Module()
    @eval M begin
        f(x::String...) = join(x, ' ')
        f(x::Int...) = sum(x)
        g(c) = f(c...); const gline = @__LINE__
    end
    @test M.g(Any["cat", "dog"]) == "cat dog"
    provider = DefaultProvider()
    mif = find_method_instance(provider, M.f, (String, Vararg{String}))
    mig = find_method_instance(provider, M.g, (Vector{Any},))
    @test isempty(Cthulhu.find_caller_of(provider, mif, mig))
    candidate, lines = only(Cthulhu.find_caller_of(provider, mif, mig; allow_unspecialized=true))
    @test candidate[1] === nameof(M.g)
    @test candidate[2] === Symbol(@__FILE__)
    @test candidate[3] == 0 # depth
    @test lines == [M.gline]

    # Detection in optimized (post-inlining) code
    @noinline nicallee(x) = 2x
    midcaller(x) = nicallee(x), @__LINE__
    function outercaller(x)
        val, line2 = midcaller(x); line1 = @__LINE__
        val = sum([val])     # FIXME: without this line, `lookup` fails because codeinst.inferred === nothing
        return val, line1, line2
    end
    _, line1, line2 = outercaller(7)
    provider = DefaultProvider()
    micaller = find_method_instance(provider, outercaller, (Int,))
    micallee = find_method_instance(provider, nicallee, (Int,))
    callerinfo = Cthulhu.find_caller_of(provider, micallee, micaller)
    @test length(callerinfo) == 2
    info, lines = callerinfo[1]
    @test info == (:outercaller, Symbol(@__FILE__), 0)
    @test lines == [line1]
    info, lines = callerinfo[2]
    @test info == (:midcaller, Symbol(@__FILE__), 1)
    @test lines == [line2]
end

@testset "ascend interface" begin
    m = Module()
    @eval m begin
        using Cthulhu
        struct FunnyMI end
        struct HasName
            name::Symbol
        end
        Cthulhu.method(::FunnyMI) = HasName(:funny)
        funny(c::Char) = "haha"
        Cthulhu.specTypes(::FunnyMI) = Tuple{typeof(funny),Char}
    end

    io = IOBuffer()
    @test Cthulhu.callstring(io, m.FunnyMI()) == "funny(::Char)"
end

##
# Cthulhu config test
##
let config = Cthulhu.CthulhuConfig(enable_highlighter=false)
    for lexer in ["llvm", "asm"]
        @test sprint() do io
            Cthulhu.highlight(io, "INPUT", lexer, config)
        end == "INPUT\n"
    end
end

let config = Cthulhu.CthulhuConfig(
    highlighter = `I_am_hoping_this_command_does_not_exist`,
    enable_highlighter = true,
)
    for lexer in ["julia"]
        @test begin
            @test_logs (:warn, r"Highlighter command .* does not exist.") begin
                sprint() do io
                    Cthulhu.highlight(io, "INPUT", lexer, config)
                end == "INPUT\n"
            end
        end
    end
end

let config = Cthulhu.CthulhuConfig(
    # Implementing `cat` in Julia:
    highlighter = `$(Base.julia_cmd()) -e "write(stdout, read(stdin))" --`,
    enable_highlighter = true,
)
    for lexer in ["llvm", "asm"]
        @test sprint() do io
            Cthulhu.highlight(io, "INPUT", lexer, config)
        end == "INPUT\n"
        @test sprint() do io
            Cthulhu.highlight(io, "INPUT\n", lexer, config)
        end == "INPUT\n"
    end
end

# https://github.com/JuliaDebug/Cthulhu.jl/issues/152
let callsites = @find_callsites_by_ftt optimize=false replace("s", "a"=>"b")
    @test !isempty(callsites)
end
function issue152_another(t)
    s = 0
    for a in t
        s += a
    end
    return s
end
let callsites = find_callsites_by_ftt(issue152_another, (Tuple{Float64,Vararg{Float64}},); optimize=false)
    @test !isempty(callsites)
end

@testset "Bookmarks" begin
    provider, mi, ci, result = cthulhu_info(sqrt, (Float64,))
    config = setproperties(CONFIG, (; view = :typed, optimize = true))
    b = Cthulhu.Bookmark(provider, config, ci)

    @testset "code_typed(bookmark)" begin
        ci, rt = code_typed(b)
        @test ci isa CodeInfo
        @test rt isa Type
    end

    @testset "code_typed(bookmark; optimize = false)" begin
        ci, rt = code_typed(b; optimize = false)
        @test ci isa CodeInfo
        @test rt isa Type
    end

    @testset "show(io, bookmark)" begin
        str = sprint(io -> show(io, "text/plain", b))
        @test occursin("Cthulhu.Bookmark (world: ", str)
    end

    @testset "show(io, [bookmark])" begin
        # Test that it does not print the full IR:
        str = sprint(io -> show(io, "text/plain", [b]))
        @test contains(str, "\n invoke sqrt(::Float64)::Float64 (world:")
        @test !occursin("Cthulhu.Bookmark (world: ", str)
    end

    @testset "Smoke tests" begin
        @test code_warntype(devnull, b) isa Any
        @test code_llvm(devnull, b) isa Any
        @test code_native(devnull, b) isa Any
    end
end

## --- Test loading and saving of preferences

@testset "preferences" begin
    # Test that load and save are able to set state
    _Cthulhu.CONFIG = setproperties(CONFIG, (; enable_highlighter = true, debuginfo = :none))
    _Cthulhu.save_config!(config)
    _Cthulhu.CONFIG = setproperties(CONFIG, (; enable_highlighter = false, debuginfo = :compact))
    @test _Cthulhu.CONFIG.enable_highlighter === false
    @test _Cthulhu.CONFIG.debuginfo === :compact

    _Cthulhu.read_config!()
    @test _Cthulhu.CONFIG.enable_highlighter === true
    @test _Cthulhu.CONFIG.debuginfo === :none
end

Base.@constprop :none sin_noconstprop(x) = sin(x)
function remarks_dced(x)
    if isa(x, Float64)
        v, w = sin(x), sin_noconstprop(x)
    else
        v, w = sin_noconstprop(x), sin(x)
    end
    return v
end
@testset "per-statement remarks" begin
    provider, mi, ci, result = cthulhu_info(remarks_dced, (Float64,))
    src = provider.interp.unopt[ci].src
    i = only(findall(iscall((src, sin)), src.code))
    j = only(findall(iscall((src, sin_noconstprop)), src.code))
    @test i < j
    pc2remarks = provider.interp.remarks[ci]
    @test any(pc2remarks) do (pc, msg)
        pc == j && occursin("Disabled by method parameter", msg)
    end
end

function effects_dced(x)
    if isa(x, Int)
        a = Int[]
    else
        a = Any[]
    end
    push!(a, x)
    n = Core.arraysize(a, 1)
    return a, n
end
@testset "per-statement effects" begin
    provider, mi, ci, result = cthulhu_info(effects_dced, (Int,))
    src = provider.interp.unopt[ci].src
    i1 = only(findall(iscall((src, isa)), src.code))
    i2 = only(findall(iscall((src, getindex)), src.code))
    i3 = only(findall(iscall((src, push!)), src.code))
    i4 = only(findall(iscall((src, Core.arraysize)), src.code))
    @test i1 < i2 < i3 < i4
    pc2effects = provider.interp.effects[ci]
    @test haskey(pc2effects, i1)
    @test haskey(pc2effects, i2)
    @test haskey(pc2effects, i3)
    @test haskey(pc2effects, i4)
end

@inline countvars50037(bitflags::Int, var::Int) = bitflags >> 0
let (_, _, ci, _) = cthulhu_info((Int,)) do var::Int
        # Make sure that code is cached by ensuring a non-const return type.
        x = Base.inferencebarrier(1)::Int
        countvars50037(x, var)
    end
    inferred = @atomic :monotonic ci.inferred
    @test length(inferred.ir.cfg.blocks) == 1
end

f515() = cglobal((:foo, bar))
@testset "issue #515" begin
    callsites = find_callsites_by_ftt(f515)
    @test isempty(callsites)
end

# end # module test_Cthulhu
