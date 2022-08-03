using Cthulhu
using REPL
using InteractiveUtils
using Test
using Random
using StaticArrays
using Revise

@test isempty(detect_ambiguities(Cthulhu))

include("setup.jl")

function testf_simple()
    T = rand() > 0.5 ? Int64 : Float64
    sum(rand(T, 100))
end

function empty_func(::Bool) end

isordered(::Type{T}) where {T<:AbstractDict} = false

@testset "Callsites" begin
    callsites = find_callsites_by_ftt(testf_simple)
    @test length(callsites) >= 4

    callsites = find_callsites_by_ftt(testf_simple; optimize=false)
    @test length(callsites) == 4

    callsites = find_callsites_by_ftt(empty_func, Tuple{Bool}; optimize=true)
    @test isempty(callsites)

    # handle pure
    callsites = find_callsites_by_ftt(iterate, Tuple{SVector{3,Int}, Tuple{SOneTo{3}}}; optimize=false)
    @test occursin("< pure >", string(callsites[1]))
    @test occursin(r"< constprop > getindex\(::.*Const.*,::.*Const\(1\)\)::.*Const\(1\)", string(callsites[2]))
    callsites = @eval find_callsites_by_ftt(; optimize=false) do
        length($(QuoteNode(Core.svec(0,1,2))))
    end
    @test occursin("< pure >", string(callsites[1]))

    # Some weird methods get inferred
    callsites = find_callsites_by_ftt(iterate, (Base.IdSet{Any}, Union{}); optimize=false)
    @test callsites[1].info isa Cthulhu.ConstPropCallInfo

    @static if Cthulhu.EFFECTS_ENABLED
        effects = Cthulhu.get_effects(callsites[1].info)
        @test !Core.Compiler.is_foldable(effects)
        @test !Core.Compiler.is_consistent(effects)
        @test Core.Compiler.is_effect_free(effects)
        @test Core.Compiler.is_nothrow(effects)
        @test Core.Compiler.is_terminates(effects)
    end
end

@testset "Expr heads" begin
    # Check that the Expr head (:invoke or :call) is preserved
    @noinline twice(x::Real) = 2x
    calltwice(c) = twice(c[1])

    callsites = find_callsites_by_ftt(calltwice, Tuple{Vector{Float64}})
    @test length(callsites) == 1 && callsites[1].head === :invoke
    io = IOBuffer()
    print(io, callsites[1])
    @test occursin("invoke twice(::Float64)::Float64", String(take!(io)))

    callsites = find_callsites_by_ftt(calltwice, Tuple{Vector{AbstractFloat}})
    @test length(callsites) == 1 && callsites[1].head === :call
    io = IOBuffer()
    print(io, callsites[1])
    @test occursin("call twice(::AbstractFloat)", String(take!(io)))

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
    S = $(Core.Compiler.return_type)(+, Tuple{Int, Int})
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

# # TODO run this testset in a separate process
# # julia --check-bounds=auto --code-coverage=none
# @testset "DCE & boundscheck" begin
#     M = Module()
#     @eval M begin
#         Base.@propagate_inbounds function f(x)
#             @boundscheck error()
#         end
#         g(x) = @inbounds f(x)
#         h(x) = f(x)
#     end
#
#     let
#         (_,CI, _, _, _, _) = process(M.g, Tuple{Vector{Float64}})
#         @test all(CI.stmts.inst) do stmt
#             isa(stmt, Core.GotoNode) || (isa(stmt, Core.ReturnNode) && isdefined(stmt, :val))
#         end
#     end
#
#     let
#         (_,CI, _, _, _, _) = process(M.h, Tuple{Vector{Float64}})
#         @test count(!isnothing, CI.stmts.inst) == 2
#         stmt = CI.stmts.inst[end]
#         @test isa(stmt, Core.ReturnNode) && !isdefined(stmt, :val)
#     end
# end

# Something with many methods, but enough to be under the match limit
g_matches(a::Int, b::Int) = a+b
g_matches(a::Float64, b::Float64) = a+b
f_matches(a, b) = g_matches(a, b)
let callsites = find_callsites_by_ftt(f_matches, Tuple{Any, Any}; optimize=false)
    @test length(callsites) == 1
    callinfo = callsites[1].info
    @test callinfo isa Cthulhu.MultiCallInfo
    @static Cthulhu.EFFECTS_ENABLED && @test Cthulhu.get_effects(callinfo) |> Core.Compiler.is_foldable
    io = IOBuffer()
    Cthulhu.show_callinfo(io, callinfo)
    @test occursin(r"→ g_matches\(::Any, ?::Any\)::Union{Float64, ?Int\d+}", String(take!(io)))
end

@testset "wrapped callinfo" begin
    let
        m = Module()
        @eval m begin
            # mutually recursive functions
            f(a) = g(a)
            g(a) = somecode::Bool ? h(a) : a
            h(a) = f(Type{a})
        end

        # make sure we form `UncachedCallInfo` so that we won't try to retrieve non-existing cache
        callsites = @find_callsites_by_ftt m.f(Int)
        @test length(callsites) == 1
        ci = first(callsites).info
        @test isa(ci, Cthulhu.UncachedCallInfo)
        @static if Cthulhu.EFFECTS_ENABLED
            effects = Cthulhu.get_effects(ci)
            @test !Core.Compiler.is_consistent(effects)
            @test Core.Compiler.is_effect_free(effects)
            @test !Core.Compiler.is_nothrow(effects)
            @test !Core.Compiler.is_terminates(effects)
        end
        @test Cthulhu.is_callsite(ci, ci.wrapped.mi)
        io = IOBuffer()
        show(io, first(callsites))
        @test occursin("< uncached >", String(take!(io)))
        # TODO do some test with `LimitedCallInfo`, but they happen at deeper callsites
    end
end

@testset "ConstPropCallInfo" begin
    @testset "union-split constant-prop'ed callsites" begin
        # constant prop' on all the splits
        let callsites = (@eval Module() begin
                struct F32
                    val::Float32
                    _v::Int
                end
                struct F64
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
            @test length(callsites) == 1                                        # getindex(::Union{Vector{Any}, Const(tuple(1,nothing))}, ::Const(1))
            callinfo = callsites[1].info
            @test isa(callinfo, Cthulhu.MultiCallInfo)
            callinfos = callinfo.callinfos
            @test length(callinfos) == 2
            @test count(ci->isa(ci, Cthulhu.MICallInfo), callinfos) == 1        # getindex(::Vector{Any}, ::Const(1))
            @test count(ci->isa(ci, Cthulhu.ConstPropCallInfo), callinfos) == 1 # getindex(::Const(tuple(1,nothing)), ::Const(1))
        end

        let callsites = (@eval Module() begin
                struct F32
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

    @static isdefined(Core.Compiler, :ConstResult) && @testset "ConstResult" begin
        # constant prop' on all the splits
        let callsites = (@eval Module() begin
                Base.@assume_effects :terminates_locally function issue41694(x)
                    res = 1
                    1 < x < 20 || throw("bad")
                    while x > 1
                        res *= x
                        x -= 1
                    end
                    return res
                end

                $find_callsites_by_ftt(; optimize = false) do
                    issue41694(12)
                end
            end)
            callinfo = only(callsites).info
            @test isa(callinfo, Cthulhu.ConcreteCallInfo)
            @test Cthulhu.get_rt(callinfo) == Core.Const(factorial(12))
            @test Cthulhu.get_effects(callinfo) |> Core.Compiler.is_foldable
            io = IOBuffer()
            print(io, only(callsites))
            @test occursin("= < concrete eval > issue41694(::Core.Const(12))", String(take!(io)))
        end
    end
end

struct SingletonPureCallable{N} end

@testset "PureCallInfo" begin
    c1 = Cthulhu.PureCallInfo(Any[typeof(sin), Float64], Float64)
    s = sprint(Cthulhu.show_callinfo, c1)
    @test s == "sin(::Float64)::Float64"

    c2 =  Cthulhu.PureCallInfo(Any[SingletonPureCallable{1}, Float64], Float64)
    s = sprint(Cthulhu.show_callinfo, c2)
    @test s == "SingletonPureCallable{1}()(::Float64)::Float64"

    @static if Cthulhu.EFFECTS_ENABLED
        @test Cthulhu.get_effects(c1) |> Core.Compiler.is_total
        @test Cthulhu.get_effects(c2) |> Core.Compiler.is_total
    end
end

@testset "ReturnTypeCallInfo" begin
    only_ints(::Integer) = 1

    callsites = find_callsites_by_ftt(; optimize=false) do
            t1 = Base._return_type(only_ints, Tuple{Int})     # successful `return_type`
            t2 = Base._return_type(only_ints, Tuple{Float64}) # failed `return_type`
            t1, t2
        end
    @test length(callsites) == 2
    callinfo1 = callsites[1].info
    @test callinfo1 isa Cthulhu.ReturnTypeCallInfo
    @test callinfo1.vmi isa Cthulhu.MICallInfo
    io = IOBuffer()
    Cthulhu.show_callinfo(io, callinfo1)
    @test String(take!(io)) == "only_ints(::$Int)::$Int"
    io = IOBuffer()
    print(io, callsites[1])
    @test occursin("return_type < only_ints(::$Int)::$Int >", String(take!(io)))

    callinfo2 = callsites[2].info
    @test callinfo2 isa Cthulhu.ReturnTypeCallInfo
    @test callinfo2.vmi isa Cthulhu.FailedCallInfo
    io = IOBuffer()
    Cthulhu.show_callinfo(io, callinfo2)
    @test String(take!(io)) == "only_ints(::Float64)::Union{}"
    io = IOBuffer()
    print(io, callsites[2])
    @test occursin("return_type < only_ints(::Float64)::Union{} >", String(take!(io)))

    @static if Cthulhu.EFFECTS_ENABLED
        @test Cthulhu.get_effects(callinfo1) |> Core.Compiler.is_total
        @test Cthulhu.get_effects(callinfo2) |> Core.Compiler.is_total
    end
end

@testset "OCCallInfo" begin
    let callsites = find_callsites_by_ftt((Int,Int,); optimize=false) do a, b
            oc = Base.Experimental.@opaque b -> sin(a) + cos(b)
            oc(b)
        end
        @test length(callsites) == 1
        callinfo = only(callsites).info
        @test callinfo isa Cthulhu.OCCallInfo
        @static if Cthulhu.EFFECTS_ENABLED
            @test Cthulhu.get_effects(callinfo) |> !Core.Compiler.is_total
            # TODO not sure what these effects are (and neither is Base.infer_effects yet)
        end
        @test callinfo.ci.rt === Base.return_types((Int,Int)) do a, b
            sin(a) + cos(b)
        end |> only === Float64

        buf = IOBuffer()
        Cthulhu.show_callinfo(buf, callinfo.ci)
        s = "opaque closure(::$Int)::$Float64"
        @test String(take!(buf)) == s
        print(buf, only(callsites))
        @test occursin("< opaque closure call > $s", String(take!(buf)))
    end

    # const-prop'ed OC callsite
    @static hasfield(CC.OpaqueClosureCallInfo, :result) && let callsites = find_callsites_by_ftt((Int,); optimize=false) do a
            oc = Base.Experimental.@opaque Base.@constprop :aggressive b -> sin(b)
            oc(42)
        end
        @test length(callsites) == 1
        callinfo = only(callsites).info
        @test callinfo isa Cthulhu.OCCallInfo
        inner = callinfo.ci
        @test inner isa Cthulhu.ConstPropCallInfo

        buf = IOBuffer()
        Cthulhu.show_callinfo(buf, callinfo.ci)
        s = "opaque closure(::$(Core.Compiler.Const(42)))::$(Core.Compiler.Const(sin(42)))"
        @test String(take!(buf)) == s
        print(buf, only(callsites))
        @test occursin("< opaque closure call > $s", String(take!(buf)))
    end
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
    let callsites = find_callsites_by_ftt((Int,); optimize=false) do n
            Base.@invoke invoke_call(n::Integer)
        end
        callsite = only(callsites)
        info = callsite.info
        @test isa(info, Cthulhu.InvokeCallInfo)
        @static Cthulhu.EFFECTS_ENABLED && Cthulhu.get_effects(info) |> Core.Compiler.is_total
        rt = Core.Compiler.Const(:Integer)
        @test info.ci.rt === rt
        buf = IOBuffer()
        show(buf, callsite)
        @test occursin("= invoke < invoke_call(::$Int)::$rt >", String(take!(buf)))
    end
    let callsites = find_callsites_by_ftt((Int,); optimize=false) do n
            Base.@invoke invoke_call(n::Int)
        end
        callsite = only(callsites)
        info = callsite.info
        @test isa(info, Cthulhu.InvokeCallInfo)
        @static Cthulhu.EFFECTS_ENABLED && Cthulhu.get_effects(info) |> Core.Compiler.is_total
        @test info.ci.rt === Core.Compiler.Const(:Int)
    end

    # const prop' callsite
    @static hasfield(CC.InvokeCallInfo, :result) && let callsites = find_callsites_by_ftt((Any,); optimize=false) do a
            Base.@invoke invoke_constcall(a::Any, true::Bool)
        end
        callsite = only(callsites)
        info = callsite.info
        @test isa(info, Cthulhu.InvokeCallInfo)
        @static Cthulhu.EFFECTS_ENABLED && Cthulhu.get_effects(info) |> Core.Compiler.is_total
        inner = info.ci
        @test isa(inner, Cthulhu.ConstPropCallInfo)
        rt = Core.Compiler.Const(Any)
        @test inner.result.result === rt
        buf = IOBuffer()
        show(buf, callsite)
        @test occursin("= invoke < invoke_constcall(::Any,::$(Core.Compiler.Const(true)))::$rt", String(take!(buf)))
    end
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
        @test all(cs->cs.info isa Cthulhu.MICallInfo, callsites)
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
        mis = map(Cthulhu.get_mi, callinfos)
        @test any(mi->mi.def.name === :cos, mis)
        @test any(mi->mi.def.name === :sin, mis)
    else
        @test all(cs->cs.info isa Cthulhu.MICallInfo, callsites)
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
    @test checklim(str -> str == "", 1, 'a')
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
            mi = cs.info.mi
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
        @test occursin("::Core.Const(-1)", str)
    end
    checklim(55, cs.info) do str
        @test !occursin("Array{…}", str)
        @test !occursin("::Core.Const(-1)", str)
    end
    checklim(40, cs.info) do str
        @test occursin("Array{…}", str)
        @test occursin("::Core.Const(-1)", str)
    end
    checklim(25, cs.info) do str
        @test occursin("Array{…}", str)
        @test !occursin("::Core.Const(-1)", str)
    end
    checklim(8, cs.info) do str
        @test str == "foo(…)"
    end
    checklim(4, cs.info) do str
        @test str == "…"
    end
end

@testset "MaybeUndef" begin
    function undef(b::Bool)
        b || @goto final_step
        str = randstring(8)
        @label final_step
        return str*"end"
    end
    @test isa(undef(true), String)
    @test_throws UndefVarError undef(false)
    cs = find_callsites_by_ftt(undef, Tuple{Bool})[end]
    @test cs.head === :invoke
    @test cs.info.mi.def == which(string, (String,String))
end

like_cat(dims, xs::AbstractArray{T}...) where T = like_cat_t(T, xs...; dims=dims)
like_cat_t(::Type{T}, xs...; dims) where T = T

let callsites = find_callsites_by_ftt(like_cat, Tuple{Val{3}, Vararg{Matrix{Float32}}})
    @test length(callsites) == 1
    cs = callsites[1]
    @test cs isa Cthulhu.Callsite
    mi = cs.info.mi
    @test mi.specTypes.parameters[4] === Type{Float32}
end

@testset "warntype variables" begin
    src, rettype = code_typed(identity, (Any,); optimize=false)[1]
    io = IOBuffer()
    ioctx = IOContext(io, :color=>true)
    Cthulhu.cthulhu_warntype(ioctx, :none, src, rettype, nothing)
    str = String(take!(io))
    @test occursin("x\e[91m\e[1m::Any\e[22m\e[39m", str)
end

@testset "Limit printing (issue #94)" begin
    m = Module()
    @eval m begin
        const x = collect(1:1000)
        f1() = x
        function f2()
            y = x
            return sum(y)
        end
    end
    function doprint(f)
        interp, mi = Cthulhu.mkinterp(NativeInterpreter(), f, ())
        (; src, rt) = Cthulhu.lookup(interp, mi, true)
        io = IOBuffer()
        Cthulhu.cthulhu_typed(io, :none, src, rt, mi; iswarn=false)
        return String(take!(io))
    end
    @test occursin("invoke f1()::…\n", doprint(getfield(m, :f1)))
    str = doprint(getfield(m, :f2))
    @test occursin("x::Core.Const([1, 2, 3", str)
    @test !occursin("500,", str)
end

@testset "Issue #132" begin
    f132(w, dim) = [i == dim ? w[i]/2 : w[i]/1 for i in eachindex(w)]
    interp, mi = Cthulhu.mkinterp(NativeInterpreter(), f132, (Vector{Int}, Int))
    @test isa(mi, Core.MethodInstance)   # just check that the above succeeded
end

@testset "@interp" begin
    finterp1(x) = 2
    (interp, mi) = Cthulhu.@interp finterp1(5)
    @test isa(mi, Core.MethodInstance)

    finterp2(x, y) = string(x, y)
    (interp, mi) = Cthulhu.@interp finterp2("hi", " there")
    @test isa(mi, Core.MethodInstance)

    finterp3(x, y, z) = (x + y) / z
    tt = Tuple{typeof(finterp3), Int64, Int64, Float64}
    (interp, mi) = Cthulhu.mkinterp(tt)
    @test isa(mi, Core.MethodInstance)
end

## Functions for "backedges & treelist"
# The printing changes when the functions are defined inside the testset
fbackedge1() = 1
fbackedge2(x) = x > 0 ? fbackedge1() : -fbackedge1()
fst1(x) = backtrace()
@inline fst2(x) = fst1(x)
@noinline fst3(x) = fst2(x)
@inline fst4(x) = fst3(x)
fst5(x) = fst4(x)

@testset "backedges and treelist" begin
    @test fbackedge2(0.2) == 1
    @test fbackedge2(-0.2) == -1
    mspec = @which(fbackedge1()).specializations
    mi = isa(mspec, Core.SimpleVector) ? mspec[1] : mspec.func
    root = Cthulhu.treelist(mi)
    @test Cthulhu.count_open_leaves(root) == 2
    @test root.data.callstr == "fbackedge1()"
    @test root.children[1].data.callstr == " fbackedge2(::Float64)"

    # issue #114
    unspecva(@nospecialize(i::Int...)) = 1
    @test unspecva(1, 2) == 1
    mi = something(first(methods(unspecva)).specializations...)
    root = Cthulhu.treelist(mi)
    @test occursin("Vararg", root.data.callstr)

    # Test highlighting and other printing
    mi = Cthulhu.get_specialization(:, Tuple{T, T} where T<:Integer)
    root = Cthulhu.treelist(mi)
    @test occursin("\e[31m::T\e[39m", root.data.callstr)
    mi = Cthulhu.get_specialization(Vector{Int}, Tuple{typeof(undef), Int})
    io = IOBuffer()
    @test Cthulhu.callstring(io, mi) == "Vector{$Int}(::UndefInitializer, ::$Int)"
    mi = Cthulhu.get_specialization(similar, Tuple{Type{Vector{T}}, Dims{1}} where T)
    @test occursin(r"31m::Type", Cthulhu.callstring(io, mi))

    # treelist for stacktraces
    tree = Cthulhu.treelist(fst5(1.0))
    @test match(r"fst1 at .*:\d+ => fst2 at .*:\d+ => fst3\(::Float64\) at .*:\d+", tree.data.callstr) !== nothing
    @test length(tree.children) == 1
    child = tree.children[1]
    @test match(r" fst4 at .*:\d+ => fst5\(::Float64\) at .*:\d+", child.data.callstr) !== nothing

    # issue #184
    tree = Cthulhu.treelist(similar(fst5(1.0), 0))
    @test isempty(tree.data.callstr)
    @test isempty(Cthulhu.callstring(io, similar(stacktrace(fst5(1.0)), 0)))
    @test Cthulhu.instance(similar(stacktrace(fst5(1.0)), 0)) === Core.Compiler.Timings.ROOTmi
end

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
    micaller = Cthulhu.get_specialization(caller, Tuple{Int})
    micallee_Int = Cthulhu.get_specialization(callee, Tuple{Int})
    micallee_Float64 = Cthulhu.get_specialization(callee, Tuple{Float64})
    info, lines = only(Cthulhu.find_caller_of(NativeInterpreter(), micallee_Int, micaller))
    @test info == (:caller, Symbol(@__FILE__), 0) && lines == [line1, line3]
    info, lines = only(Cthulhu.find_caller_of(NativeInterpreter(), micallee_Float64, micaller))
    @test info == (:caller, Symbol(@__FILE__), 0) && lines == [line2]

    M = Module()
    @eval M begin
        f(x::String...) = join(x, ' ')
        f(x::Int...) = sum(x)
        g(c) = f(c...); const gline = @__LINE__
    end
    @test M.g(Any["cat", "dog"]) == "cat dog"

    mif = Cthulhu.get_specialization(M.f, Tuple{String, Vararg{String}})
    mig = Cthulhu.get_specialization(M.g, Tuple{Vector{Any}})
    @test isempty(Cthulhu.find_caller_of(Cthulhu.CthulhuInterpreter(), mif, mig))
    candidate, lines = only(Cthulhu.find_caller_of(Cthulhu.CthulhuInterpreter(), mif, mig; allow_unspecialized=true))
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
    micaller = Cthulhu.get_specialization(outercaller, Tuple{Int})
    micallee = Cthulhu.get_specialization(nicallee, Tuple{Int})
    callerinfo = Cthulhu.find_caller_of(NativeInterpreter(), micallee, micaller)
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

include("codeview.jl")

@testset "Bookmarks" begin
    (interp, mi) = Cthulhu.mkinterp(NativeInterpreter(), sqrt, Tuple{Float64})
    b = Cthulhu.Bookmark(mi, interp)

    @testset "code_typed(bookmark)" begin
        ci, rt = code_typed(b)
        @test ci isa Core.Compiler.CodeInfo
        @test rt isa Type
    end

    @testset "code_typed(bookmark; optimize = false)" begin
        ci, rt = code_typed(b; optimize = false)
        @test ci isa Core.Compiler.CodeInfo
        @test rt isa Type
    end

    @testset "show(io, bookmark)" begin
        str = sprint(io -> show(io, "text/plain", b))
        @test occursin("Cthulhu.Bookmark (world: ", str)
    end

    @testset "show(io, [bookmark])" begin
        # Test that it does not print the full IR:
        str = sprint(io -> show(io, "text/plain", [b]))
        @test occursin("world:", str)
        @test !occursin("Cthulhu.Bookmark (world: ", str)
    end

    @testset "Smoke tests" begin
        @test code_warntype(devnull, b) isa Any
        @test code_llvm(devnull, b) isa Any
        @test code_native(devnull, b) isa Any
    end
end

include("terminal.jl")

using Cthulhu: MultiCallInfo, show_callinfo, CallInfo, TextWidthLimiter

@testset "printing of MultiCallInfo" begin
    # PR 226
    m = Module()
    @eval m begin
        struct Foo
            x
        end
        struct Bar end
    end
    ci = MultiCallInfo(Tuple{m.Foo, Float64}, Float64, CallInfo[])
    @test sprint(show_callinfo, ci) == "→ (::Foo)(::Float64)::Float64"
    ci = MultiCallInfo(Tuple{Union{m.Foo, m.Bar}, Float64}, Float64, CallInfo[])
    @test sprint(show_callinfo, ci) == "→ (::Union{Bar, Foo})(::Float64)::Float64"
    ci = MultiCallInfo(Tuple{typeof(+), Bool, Vararg{Rational{BigInt}, 2}}, Float64, CallInfo[])
    @test sprint(show_callinfo, ci) ==
        "→ +(::Bool,::Rational{BigInt},::Rational{BigInt})::Float64"
    ci = MultiCallInfo(Tuple{typeof(+), Bool, Vararg{Rational{BigInt}, 4}}, Float64, CallInfo[])
    @test sprint(io -> show_callinfo(TextWidthLimiter(io, 80), ci)) ==
        "→ +(::Bool,::Rational{…},::Rational{…},::Rational{…},::Rational{…})::Float64"
    ci = MultiCallInfo(Tuple{m.Foo, Vararg{Float64, 30}}, Float64, CallInfo[])
    @test sprint(io -> show_callinfo(TextWidthLimiter(io, 80), ci)) ==
        "→ (::Foo)(…,…,…,…,…,…,…,…,…,…,…,…,…,…,…,…,…,…,…,…,…,…,…,…,…,…,…,…,…,…)::…"
end

# external AbstractInterpreter integration
# ========================================

# define new `AbstractInterpreter` that satisfies the minimum interface requirements
# while managing its cache independently
macro newinterp(name)
    cachename = gensym(string(name, "Cache"))
    name = esc(name)
    quote
        struct $cachename
            dict::IdDict{MethodInstance,CodeInstance}
        end
        struct $name <: CC.AbstractInterpreter
            interp::CC.NativeInterpreter
            cache::$cachename
            $name(world = Base.get_world_counter();
                interp = CC.NativeInterpreter(world),
                cache = $cachename(IdDict{MethodInstance,CodeInstance}())
                ) = new(interp, cache)
        end
        CC.InferenceParams(interp::$name) = CC.InferenceParams(interp.interp)
        CC.OptimizationParams(interp::$name) = CC.OptimizationParams(interp.interp)
        CC.get_world_counter(interp::$name) = CC.get_world_counter(interp.interp)
        CC.get_inference_cache(interp::$name) = CC.get_inference_cache(interp.interp)
        CC.code_cache(interp::$name) = WorldView(interp.cache, WorldRange(CC.get_world_counter(interp)))
        CC.get(wvc::WorldView{<:$cachename}, mi::MethodInstance, default) = get(wvc.cache.dict, mi, default)
        CC.getindex(wvc::WorldView{<:$cachename}, mi::MethodInstance) = getindex(wvc.cache.dict, mi)
        CC.haskey(wvc::WorldView{<:$cachename}, mi::MethodInstance) = haskey(wvc.cache.dict, mi)
        CC.setindex!(wvc::WorldView{<:$cachename}, ci::CodeInstance, mi::MethodInstance) = setindex!(wvc.cache.dict, ci, mi)
    end
end

# `OverlayMethodTable`
# --------------------
import Base.Experimental: @MethodTable, @overlay

@newinterp MTOverlayInterp
@MethodTable(OverlayedMT)
@static if v"1.8-beta2" <= VERSION < v"1.9-" || VERSION >= v"1.9.0-DEV.120"
CC.method_table(interp::MTOverlayInterp) = CC.OverlayMethodTable(CC.get_world_counter(interp), OverlayedMT)
else # if v"1.8-beta2" <= VERSION < v"1.9-" || VERSION >= v"1.9.0-DEV.120"
CC.method_table(interp::MTOverlayInterp, sv::CC.InferenceState) = CC.OverlayMethodTable(CC.get_world_counter(interp), OverlayedMT)
end # if v"1.8-beta2" <= VERSION < v"1.9-" || VERSION >= v"1.9.0-DEV.120"
@overlay OverlayedMT sin(x::Float64) = 1

let
    interp, mi = Cthulhu.mkinterp((Int,); interp=MTOverlayInterp()) do x
        sin(x)
    end
    inferred = interp.unopt[mi]
    @test inferred.rt === Core.Const(1)
end

## --- Test loading and saving of preferences

@testset "preferences" begin
    # Test that load and save are able to set state
    Cthulhu.CONFIG.enable_highlighter = true
    Cthulhu.CONFIG.debuginfo = :none
    Cthulhu.save_config!(Cthulhu.CONFIG)

    Cthulhu.CONFIG.enable_highlighter = false
    Cthulhu.CONFIG.debuginfo = :compact
    @test Cthulhu.CONFIG.debuginfo === :compact
    @test !Cthulhu.CONFIG.enable_highlighter

    Cthulhu.read_config!(Cthulhu.CONFIG)
    @test Cthulhu.CONFIG.debuginfo === :none
    @test Cthulhu.CONFIG.enable_highlighter
end
