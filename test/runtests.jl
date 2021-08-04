using Cthulhu
using REPL
using InteractiveUtils
using Test
using Random
using StaticArrays
using Revise

@test isempty(detect_ambiguities(Cthulhu))

function firstassigned(specializations::Core.SimpleVector)
    # the methodinstance may not be first (annoying)
    for i = 1:length(specializations)
        if isassigned(specializations, i)
            return specializations[i]
        end
    end
    return nothing
end
function firstassigned(specializations)
    mis = []
    Base.visit(specializations) do mi
        isempty(mis) && push!(mis, mi)
    end
    return mis[1]
end

function process(@nospecialize(f), @nospecialize(TT=()); optimize=true)
    (interp, mi) = Cthulhu.mkinterp(f, TT)
    (ci, rt, infos, slottypes) = Cthulhu.lookup(interp, mi, optimize)
    if ci !== nothing
        ci = Cthulhu.preprocess_ci!(ci, mi, optimize, Cthulhu.CthulhuConfig(dead_code_elimination=true))
    end
    interp, ci, infos, mi, rt, slottypes
end

function find_callsites_by_ftt(@nospecialize(f), @nospecialize(TT=Tuple{}); optimize=true)
    interp, ci, infos, mi, _, slottypes = process(f, TT; optimize)
    ci === nothing && return Cthulhu.Callsite[]
    callsites = Cthulhu.find_callsites(interp, ci, infos, mi, slottypes, optimize)
    return callsites
end

macro find_callsites_by_ftt(ex0...)
    return InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :find_callsites_by_ftt, ex0)
end

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

    # Broken stuff in Julia
    @test_broken find_callsites_by_ftt(Core.Compiler._limit_type_size, Tuple{Any, Type{Any}, Core.SimpleVector, Int, Int})  # ssair/ir.jl bug
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

if Base.JLOptions().check_bounds ∈ (0, 2)
    @testset "DCE & boundscheck" begin
        Base.@propagate_inbounds function f(x)
            @boundscheck error()
        end
        g(x) = @inbounds f(x)
        h(x) = f(x)

        (_,CI, _, _, _, _) = process(g, Tuple{Vector{Float64}})
        @test all(CI.stmts.inst) do stmt
            isa(stmt, Core.GotoNode) || (isa(stmt, Core.ReturnNode) && isdefined(stmt, :val)) || Base.Meta.isexpr(stmt, :code_coverage_effect, 0)
        end

        (_,CI, _, _, _, _) = process(h, Tuple{Vector{Float64}})
        i = 1
        while CI.stmts.inst[i] === nothing || Base.Meta.isexpr(CI.stmts.inst[i], :code_coverage_effect, 0)
            i += 1
        end
        @test length(CI.stmts) - i + 1 == 2
        stmt = CI.stmts.inst[end]
        @test isa(stmt, Core.ReturnNode) && !isdefined(stmt, :val)
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
    io = IOBuffer()
    Cthulhu.show_callinfo(io, callinfo)
    @test occursin(r"#g_matches\(::Any, ?::Any\)::Union{Float64, ?Int\d+}", String(take!(io)))
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
        @test Cthulhu.is_callsite(ci, ci.wrapped.mi)
        io = IOBuffer()
        show(io, first(callsites))
        @test occursin("< uncached >", String(take!(io)))
        # TODO do some test with `LimitedCallInfo`, but they happen at deeper callsites
    end
end

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
        @test occursin("= call #getproperty", String(take!(io)))
    end

    # successful and unsuccessful constant prop'
    let callsites = (@eval Module() begin
            struct F32
                val::Float32
                _v::Int
            end
            struct FZero end
            Base.getproperty(::FZero, ::Symbol) = 0.0 # constant prop' won't happen here

            $find_callsites_by_ftt((Union{F32,FZero},); optimize = false) do f
                f.val
            end
        end)
        @test length(callsites) == 1
        callinfo = callsites[1].info
        @test isa(callinfo, Cthulhu.MultiCallInfo)
        callinfos = callinfo.callinfos
        @test length(callinfos) == 2
        @test count(ci->isa(ci, Cthulhu.MICallInfo), callinfos) == 1
        @test count(ci->isa(ci, Cthulhu.ConstPropCallInfo), callinfos) == 1
    end

    callsites = (@eval Module() begin
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

# Failed return_type
only_ints(::Integer) = 1
return_type_failure(::T) where T = Base._return_type(only_ints, Tuple{T})
let callsites = find_callsites_by_ftt(return_type_failure, Tuple{Float64}, optimize=false)
    @test length(callsites) == 1
    callinfo = callsites[1].info
    @test callinfo isa Cthulhu.ReturnTypeCallInfo
    callinfo = callinfo.called_mi
    @test callinfo isa Cthulhu.MultiCallInfo
    io = IOBuffer()
    Cthulhu.show_callinfo(io, callinfo)
    @test String(take!(io)) == "#return_type(::typeof(only_ints),::Type{Tuple{Float64}})::Core.Const(Union{})"
    io = IOBuffer()
    print(io, callsites[1])
    @test occursin("return_type < #return_type", String(take!(io)))
    @test length(callinfo.callinfos) == 0
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

@testset "invoke" begin
    m = Module()
    @eval m begin
        f(a::Integer) = :Integer
        f(a::Int) = :Int
    end

    io = IOBuffer()
    let
        callsites = @eval m begin
            $find_callsites_by_ftt(; optimize=false) do
                Base.@invoke f(0::Integer)
            end
        end
        @test any(callsites) do callsite
            info = callsite.info
            isa(info, Cthulhu.InvokeCallInfo) && print(io, callsite)
            isa(info, Cthulhu.InvokeCallInfo) && info.ci.rt === Core.Compiler.Const(:Integer)
        end
    end
    @test occursin("= invoke < f(::Int", String(take!(io)))

    let
        callsites = @eval m begin
            $find_callsites_by_ftt(; optimize=false) do
                Base.@invoke f(0::Int)
            end
        end
        @test any(callsites) do callsite
            info = callsite.info
            isa(info, Cthulhu.InvokeCallInfo) && info.ci.rt === Core.Compiler.Const(:Int)
        end
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
        @test any(mi->mi.def.name == :cos, mis)
        @test any(mi->mi.def.name == :sin, mis)
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
        @test any(mi->mi.def.name == :cos, mis)
        @test any(mi->mi.def.name == :sin, mis)
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
        interp, mi = Cthulhu.mkinterp(f, ())
        src, rt = Cthulhu.lookup(interp, mi, true)
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
    interp, mi = Cthulhu.mkinterp(f132, (Vector{Int}, Int))
    @test isa(mi, Core.MethodInstance)   # just check that the above succeeded
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
    callee(x) = 2x
    function caller(x)
        val = 0.0
        val += callee(3); line1 = @__LINE__
        val += callee(3.0); line2 = @__LINE__
        val += callee(x); line3 = @__LINE__
        val = sum([val])     # FIXME: without this line, `lookup` fails because codeinst.inferred === nothing
        return val, line1, line2, line3
    end
    _, line1, line2, line3 = caller(7)
    micaller = Cthulhu.get_specialization(caller, Tuple{Int})
    micallee_Int = Cthulhu.get_specialization(callee, Tuple{Int})
    micallee_Float4 = Cthulhu.get_specialization(callee, Tuple{Float64})
    info, lines = only(Cthulhu.find_caller_of(micallee_Int, micaller))
    @test info == (:caller, Symbol(@__FILE__), 0) && lines == [line1, line3]
    info, lines = only(Cthulhu.find_caller_of(micallee_Float4, micaller))
    @test info == (:caller, Symbol(@__FILE__), 0) && lines == [line2]

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
    callerinfo = Cthulhu.find_caller_of(micallee, micaller)
    @test length(callerinfo) == 2
    info, lines = callerinfo[1]
    @test info == (:outercaller, Symbol(@__FILE__), 0)
    @test lines == [line1]
    info, lines = callerinfo[2]
    @test info == (:midcaller, Symbol(@__FILE__), 1)
    @test lines == [line2]
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
    for lexer in ["llvm", "asm"]
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

# NOTE setup to for `cthulhu_ast`
include("sandbox.jl")
using .CthulhuTestSandbox
Revise.track(CthulhuTestSandbox, normpath(@__DIR__, "sandbox.jl"))

@testset "printer test" begin
    _, src, infos, mi, rt, slottypes = process(testf_revise);
    tf = (true, false)

    @testset "codeview: $codeview" for codeview in Cthulhu.CODEVIEWS
        if !@isdefined(Revise)
            codeview == Cthulhu.cthulhu_ast && continue
        end
        @testset "optimize: $optimize" for optimize in tf
            @testset "debuginfo: $debuginfo" for debuginfo in instances(Cthulhu.DInfo.DebugInfo)
                params = Cthulhu.current_params()
                config = Cthulhu.CONFIG

                io = IOBuffer()
                codeview(io, mi, optimize, debuginfo, params, config)
                @test !isempty(String(take!(io))) # just check it works
            end
        end
    end

    @testset "debuginfo: $debuginfo" for debuginfo in instances(Cthulhu.DInfo.DebugInfo)
        @testset "iswarn: $iswarn" for iswarn in tf
            @testset "hide_type_stable: $hide_type_stable" for hide_type_stable in tf
                @testset "inline_cost: $inline_cost" for inline_cost in tf
                    io = IOBuffer()
                    Cthulhu.cthulhu_typed(io, debuginfo,
                        src, rt, mi;
                        iswarn, hide_type_stable, inline_cost)
                    @test !isempty(String(take!(io))) # just check it works
                end
            end
        end
    end
end

@testset "hide type-stable statements" begin
    let # optimize code
        _, src, infos, mi, rt, slottypes = @eval Module() begin
            const globalvar = Ref(42)
            $process() do
                a = sin(globalvar[])
                b = sin(undefvar)
                return (a, b)
            end
        end
        function prints(; kwargs...)
            io = IOBuffer()
            Cthulhu.cthulhu_typed(io, :none, src, rt, mi; kwargs...)
            return String(take!(io))
        end

        let # by default, should print every statement
            s = prints()
            @test occursin("globalvar", s)
            @test occursin("undefvar", s)
        end
        let # should omit type stable statements
            s = prints(; hide_type_stable=true)
            @test !occursin("globalvar", s)
            @test occursin("undefvar", s)
        end
    end

    let # unoptimize code
        _, src, infos, mi, rt, slottypes = @eval Module() begin
            const globalvar = Ref(42)
            $process(; optimize=false) do
                a = sin(globalvar[])
                b = sin(undefvar)
                return (a, b)
            end
        end
        function prints(; kwargs...)
            io = IOBuffer()
            Cthulhu.cthulhu_typed(io, :none, src, rt, mi; kwargs...)
            return String(take!(io))
        end

        let # by default, should print every statement
            s = prints()
            @test occursin("globalvar", s)
            @test occursin("undefvar", s)
        end
        let # should omit type stable statements
            s = prints(; hide_type_stable=true)
            @test !occursin("globalvar", s)
            @test occursin("undefvar", s)
        end

        # should work for warn mode
        let
            s = prints(; iswarn=true)
            @test occursin("globalvar", s)
            @test occursin("undefvar", s)
        end
        let
            s = prints(; iswarn=true, hide_type_stable=true)
            @test !occursin("globalvar", s)
            @test occursin("undefvar", s)
        end
    end
end

@testset "Bookmarks" begin
    (interp, mi) = Cthulhu.mkinterp(sqrt, Tuple{Float64})
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
