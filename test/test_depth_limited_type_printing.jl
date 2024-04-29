#=
using Revise; include(joinpath("test", "test_depth_limited_type_printing.jl"))
=#
import Cthulhu

using Cthulhu
Cthulhu.CONFIG.type_depth_limit = 2

Base.@kwdef struct Nested{A,B}
    num::Int = 1
end
bar(x) = rand() > 0.5 ? x : Any[0][1]
mysum(x) = sum(y-> bar(x.num), 1:5; init=0)
nest_val(na, nb, ::Val{1}) = Nested{na, nb}()
nest_val(na, nb, ::Val{n}) where {n} = nest_val(Nested{na, nb}, Nested{na, nb}, Val(n-1))
nest_val(na, nb, n::Int) = nest_val(na, nb, Val(n))
nest_val(n) = nest_val(1, 1, n)
const NV = nest_val(5)

# f = nest_val(5)
# a = Any[f];
# mysum(a[1]) # make sure it runs
# Cthulhu.@descend mysum(a[1]) # navigate to sum -> sum, and Nested will be there
using Test
include("setup.jl")
@testset "hide type-stable statements" begin
    let # optimize code
        # f = nest_val(5)
        # a = Any[f];
        # mysum(a[1]) # make sure it runs
        # Cthulhu.@descend mysum(a[1]) # navigate to sum -> sum, and Nested will be there
        (; src, infos, mi, rt, exct, effects, slottypes) = @eval Module() begin
            $cthulhu_info($mysum, ($(typeof(NV)),))
        end;
        function prints(; kwargs...)
            io = IOBuffer()
            ioc = IOContext(io, :maxtypedepth => Cthulhu.CONFIG.type_depth_limit)
            Cthulhu.cthulhu_typed(ioc, :none, src, rt, exct, effects, mi; kwargs...)
            return String(take!(io))
        end;

        let # by default, should print every statement
            s = prints()
            println(s)
            # @test occursin("::Nested{Nested{…}, Nested{…}}", s)
        end
    end
end
