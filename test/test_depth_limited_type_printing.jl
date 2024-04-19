#=
using Revise; include(joinpath("test", "test_depth_limited_type_printing.jl"))
=#
import Cthulhu

Base.@kwdef struct Nested{A,B}
    num::Int = 1
end
struct F49231{a,b,c,d,e,f,g}
    num::g
end;
bar(x) = rand() > 0.5 ? x : Any[0][1]
mysum(x) = sum(y-> bar(x.num), 1:5; init=0)
nest_val(na, nb, ::Val{1}) = Nested{na, nb}()
nest_val(na, nb, ::Val{n}) where {n} = nest_val(Nested{na, nb}, Nested{na, nb}, Val(n-1))
nest_val(na, nb, n::Int) = nest_val(na, nb, Val(n))
nest_val(n) = nest_val(1, 1, n)

f = nest_val(5)
a = Any[f];
mysum(a[1]) # make sure it runs
Cthulhu.@descend mysum(a[1]) # navigate to sum -> sum, and F49231 will be there
