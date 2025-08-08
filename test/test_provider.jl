# module test_provider

using Test, Cthulhu, StaticArrays, Random
using Core: Const
const CC = Cthulhu.CTHULHU_MODULE[].CC
using Cthulhu: DefaultProvider

include("setup.jl")
include("irutils.jl")

provider = DefaultProvider()

# descend(provider, +, (Int, Int))
descend(provider, exp, (Float64,))
@descend 1 + 1

# end # module test_provider
