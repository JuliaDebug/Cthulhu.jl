using Cthulhu
using Test

function process(@nospecialize(f), @nospecialize(TT); optimize=true)
    mi = Cthulhu.first_method_instance(f, TT)
    (ci, rt, slottypes) = Cthulhu.do_typeinf_slottypes(mi, optimize, Cthulhu.current_params())
    Cthulhu.preprocess_ci!(ci, mi, optimize)
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
    @test length(callsites) == 4
end

let callsites = find_callsites_by_ftt(test, Tuple{}; optimize=false)
    @test length(callsites) == 4
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
    @test !isempty(filter(c->c.info isa Cthulhu.TaskCallInfo, callsites))
end

callf(f::F, x) where F = f(x)
let callsites = find_callsites_by_ftt(callf, Tuple{Union{typeof(sin), typeof(cos)}, Float64})
    @test !isempty(callsites)
    @test first(callsites).info isa Cthulhu.MultiCallInfo
end

