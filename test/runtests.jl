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
@test length(callsites) == 4

CI, rt = code_typed(test, Tuple{}, optimize=false)[1]
callsites = Cthulhu.find_callsites(CI, Tuple{})
@test length(callsites) == 4
