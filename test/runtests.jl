using Cthulhu
using REPL
using InteractiveUtils
using Test
using Random
using StaticArrays

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

function process(@nospecialize(f), @nospecialize(TT); optimize=true)
    (interp, mi) = Cthulhu.mkinterp(f, TT)
    (ci, rt, infos, slottypes) = Cthulhu.lookup(interp, mi, optimize)
    Cthulhu.preprocess_ci!(ci, mi, optimize, Cthulhu.CthulhuConfig(dead_code_elimination=true))
    ci, infos, mi, rt, slottypes
end

function find_callsites_by_ftt(@nospecialize(f), @nospecialize(TT); optimize=true)
    ci, infos, mi, _, slottypes = process(f, TT; optimize=optimize)
    callsites = Cthulhu.find_callsites(ci, infos, mi, slottypes)
end

function test()
    T = rand() > 0.5 ? Int64 : Float64
    sum(rand(T, 100))
end

let callsites = find_callsites_by_ftt(test, Tuple{})
    @test length(callsites) >= 4
end

let callsites = find_callsites_by_ftt(test, Tuple{}; optimize=false)
    @test length(callsites) == 4
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

if Base.JLOptions().check_bounds == 0
Base.@propagate_inbounds function f(x)
    @boundscheck error()
end
g(x) = @inbounds f(x)
h(x) = f(x)

let (CI, _, _, _, _) = process(g, Tuple{Vector{Float64}})
    @test length(CI.stmts) == 3
end

let (CI, _, _, _, _) = process(h, Tuple{Vector{Float64}})
    @test length(CI.stmts) == 2
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
    @test length(callinfo.callinfos) == 0
end

# tasks
ftask() = @sync @async show(io, "Hello")
let callsites = find_callsites_by_ftt(ftask, Tuple{})
    task_callsites = filter(c->c.info isa Cthulhu.TaskCallInfo, callsites)
    @test !isempty(task_callsites)
    @test filter(c -> c.info.ci isa Cthulhu.FailedCallInfo, task_callsites) == []
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

@testset "Varargs" begin
    function fsplat(::Type{Int}, a...)
        z = zero(Int)
        for v in a
            z += v
        end
        return z
    end
    gsplat1(T::Type, a...) = fsplat(T, a...)   # does not force specialization
    hsplat1(A) = gsplat1(eltype(A), A...)
    io = IOBuffer()
    iolim = Cthulhu.TextWidthLimiter(io, 80)
    for (Atype, haslen) in ((Tuple{Tuple{Int,Int,Int}}, true),
                            (Tuple{Vector{Int}}, false),
                            (Tuple{SVector{3,Int}}, true))
        let callsites = find_callsites_by_ftt(hsplat1, Atype; optimize=false)
            @test !isempty(callsites)
            cs = callsites[end]
            @test cs isa Cthulhu.Callsite
            mi = cs.info.mi
            @test mi.specTypes.parameters[end] === (haslen ? Int : Vararg{Int})
            Cthulhu.show_callinfo(iolim, cs.info)
            @test occursin("…", String(take!(io))) != haslen
        end
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
    Cthulhu.cthulhu_warntype(ioctx, src, rettype, :none, true)
    str = String(take!(io))
    @test occursin("x\e[91m\e[1m::Any\e[22m\e[39m", str)
end

# issue #132
f132(w, dim) = [i == dim ? w[i]/2 : w[i]/1 for i in eachindex(w)]
interp, mi = Cthulhu.mkinterp(f132, (Vector{Int}, Int))
@test isa(mi, Core.MethodInstance)   # just check that the above succeeded

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

    # treelist for stacktraces
    tree = Cthulhu.treelist(fst5(1.0))
    @test match(r"fst1 at .*:\d+ => fst2 at .*:\d+ => fst3\(::Float64\) at .*:\d+", tree.data.callstr) !== nothing
    @test length(tree.children) == 1
    child = tree.children[1]
    @test match(r" fst4 at .*:\d+ => fst5\(::Float64\) at .*:\d+", child.data.callstr) !== nothing
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

###
### Printer test:
###
ci, infos, mi, rt, slottypes = process(test, Tuple{});
io = IOBuffer()
Cthulhu.cthulu_typed(io, :none, ci, rt, mi, true, false)
str = String(take!(io))
print(str)
# test by bounding the number of lines printed
@test_broken count(isequal('\n'), str) <= 50
