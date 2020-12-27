using Cthulhu
using REPL
using InteractiveUtils
using Test
using StaticArrays

function process(@nospecialize(f), @nospecialize(TT); optimize=true)
    mi = Cthulhu.first_method_instance(f, TT)
    (ci, rt, slottypes) = Cthulhu.do_typeinf_slottypes(mi, optimize, Cthulhu.current_params())
    Cthulhu.preprocess_ci!(ci, mi, optimize, Cthulhu.CthulhuConfig(dead_code_elimination=true))
    ci, mi, rt, slottypes
end

function find_callsites_by_ftt(@nospecialize(f), @nospecialize(TT); optimize=true)
    ci, mi, _, slottypes = process(f, TT; optimize=optimize)
    callsites = Cthulhu.find_callsites(ci, mi, slottypes)
end

# Testing that we don't have spurious calls from `Type`
let callsites = find_callsites_by_ftt(Base.throw_boundserror, Tuple{UnitRange{Int64},Int64})
    @test length(callsites) == 1
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

# Check that the Expr head (:invoke or :call) is preserved
@noinline twice(x::Real) = 2x
calltwice(c) = twice(c[1])
let callsites = find_callsites_by_ftt(calltwice, Tuple{Vector{Float64}})
    @test length(callsites) == 1 && callsites[1].head === :invoke
    io = IOBuffer()
    print(io, callsites[1])
    @test occursin("invoke twice(::Float64)::Float64", String(take!(io)))
end
let callsites = find_callsites_by_ftt(calltwice, Tuple{Vector{AbstractFloat}})
    @test length(callsites) == 1 && callsites[1].head === :call
    io = IOBuffer()
    print(io, callsites[1])
    @test occursin("call twice(::AbstractFloat)", String(take!(io)))
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

if VERSION >= v"1.1.0-DEV.215" && Base.JLOptions().check_bounds == 0
Base.@propagate_inbounds function f(x)
    @boundscheck error()
end
g(x) = @inbounds f(x)
h(x) = f(x)

let (CI, _, _, _) = process(g, Tuple{Vector{Float64}})
    @test length(CI.code) == 3
end

let (CI, _, _, _) = process(h, Tuple{Vector{Float64}})
    @test length(CI.code) == 2
end
end

f(a, b) = a + b
let callsites = find_callsites_by_ftt(f, Tuple{Any, Any})
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
    @test callinfo isa Cthulhu.FailedCallInfo
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

# Splatting
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
        if VERSION >= v"1.4.0-DEV.304"
            @test !isempty(callsites)
            cs = callsites[end]
            @test cs isa Cthulhu.Callsite
            mi = cs.info.mi
            @test mi.specTypes.parameters[end] === (haslen ? Int : Vararg{Int})
        end
    end
end

@testset "deoptimized calls" begin
    @noinline function f(@nospecialize(t))
        s = 0
        for k in t
            s += k
        end
        return s
    end
    g() = f((1, 2, 3))

    callsites = find_callsites_by_ftt(g, Tuple{})
    infotypes = [typeof(x.info) for x in callsites]
    @test infotypes == [Cthulhu.DeoptimizedCallInfo]

    @noinline f2(@nospecialize(a), @nospecialize(b), @nospecialize(c)) =
        (read("/dev/null"); nothing)
    h2() = __undef__::Int
    g2(a) = f2(a, h2(), 3)

    callsites = find_callsites_by_ftt(g2, Tuple{Int})
    infotypes = [typeof(x.info) for x in callsites]
    @test infotypes == [Cthulhu.DeoptimizedCallInfo]
end

@testset "warntype variables" begin
    src, rettype = code_typed(identity, (Any,); optimize=false)[1]
    io = IOBuffer()
    ioctx = IOContext(io, :color=>true)
    Cthulhu.cthulhu_warntype(ioctx, src, rettype, :none, true)
    str = String(take!(io))
    VERSION >= v"1.2" && @test occursin("x\e[91m\e[1m::Any\e[22m\e[39m", str)
end

##
# backedges & treelist
##
fbackedge1() = 1
fbackedge2(x) = x > 0 ? fbackedge1() : -fbackedge1()
@test fbackedge2(0.2) == 1
@test fbackedge2(-0.2) == -1
mspec = @which(fbackedge1()).specializations
mi = isa(mspec, Core.SimpleVector) ? mspec[1] : mspec.func
if isdefined(REPL.TerminalMenus, :ConfiguredMenu)
    root = Cthulhu.treelist(mi)
    @test Cthulhu.count_open_leaves(root) == 2
    @test root.data.callstr == "fbackedge1()"
    @test root.children[1].data.callstr == " fbackedge2(::Float64)"
else
    strs, mis = Cthulhu.treelist(mi)
    @test strs == ["fbackedge1()", " fbackedge2(::Float64)"]
end

# treelist for stacktraces
fst1(x) = backtrace()
@inline fst2(x) = fst1(x)
@noinline fst3(x) = fst2(x)
@inline fst4(x) = fst3(x)
fst5(x) = fst4(x)
tree = Cthulhu.treelist(fst5(1.0))
if isdefined(REPL.TerminalMenus, :ConfiguredMenu)
    @test match(r"fst1 at .*:\d+ => fst2 at .*:\d+ => fst3\(::Float64\) at .*:\d+", tree.data.callstr) !== nothing
    @test length(tree.children) == 1
    child = tree.children[1]
    @test match(r" fst4 at .*:\d+ => fst5\(::Float64\) at .*:\d+", child.data.callstr) !== nothing
else
    treestrings = tree[1]
    @test match(r"fst1 at .*:\d+ => fst2 at .*:\d+ => fst3\(::Float64\) at .*:\d+", treestrings[1]) !== nothing
    @test match(r" fst4 at .*:\d+ => fst5\(::Float64\) at .*:\d+", treestrings[2]) !== nothing
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

if VERSION >= v"1.1-"
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
end

###
### Printer test:
###
if VERSION >= v"1.1.0-DEV.762"
    function foo()
        T = rand() > 0.5 ? Int64 : Float64
        sum(rand(T, 100))
    end
    ci, mi, rt, st = process(foo, Tuple{});
    io = IOBuffer()
    Cthulhu.cthulu_typed(io, :none, ci, rt, mi, true, false)
    str = String(take!(io))
    print(str)
    # test by bounding the number of lines printed
    @test count(isequal('\n'), str) <= 50
end
