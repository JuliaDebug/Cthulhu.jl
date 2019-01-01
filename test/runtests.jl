using Cthulhu
using Test

# Testing that we don't have spurious calls from `Type`
CI, rt = code_typed(Base.throw_boundserror, Tuple{UnitRange{Int64},Int64})[1]
callsites = Cthulhu.find_callsites(CI, Tuple{UnitRange{Int64},Int64})
@test isempty(callsites)

function test()
    T = rand() > 0.5 ? Int64 : Float64
    sum(rand(T, 100))
end

CI, rt = code_typed(test, Tuple{})[1]
callsites = Cthulhu.find_callsites(CI, Tuple{})
@test length(callsites) == 3

CI, rt = code_typed(test, Tuple{}, optimize=false)[1]
callsites = Cthulhu.find_callsites(CI, Tuple{})
@test length(callsites) == 3

if VERSION >= v"1.1.0-DEV.215" && Base.JLOptions().check_bounds == 0
Base.@propagate_inbounds function f(x)
    @boundscheck error()
end
g(x) = @inbounds f(x)
h(x) = f(x)
CI, rt = code_typed(g, Tuple{Vector{Float64}})[1]
Cthulhu.dce!(CI,  Tuple{Vector{Float64}})
Cthulhu.dce!(CI,  Tuple{Vector{Float64}})
@test length(CI.code) == 3

CI, rt = code_typed(h, Tuple{Vector{Float64}})[1]
Cthulhu.dce!(CI,  Tuple{Vector{Float64}})
Cthulhu.dce!(CI,  Tuple{Vector{Float64}})
@test length(CI.code) == 2
end
