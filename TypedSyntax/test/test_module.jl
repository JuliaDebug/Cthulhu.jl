module TSN
using Dates

# with two uses of the same slot in the same call. Must start on line 4 (or update the corresponding test)
function simplef(a, b)
    z = a * a
    return z + b
end

function has2xa(x)
    x &= x
end
function has2xb(x)
    x -= x
    return x
end

# This is taken from the definition of `sin(::Int)` in Base, copied here for testing purposes
# in case the implementation changes
for f in (:mysin,)
    @eval function ($f)(x::Real)
        xf = float(x)
        x === xf && throw(MethodError($f, (x,)))
        return ($f)(xf)
    end
end
mysin(x::AbstractFloat) = sin(x)

function summer(list)
    s = 0                    # deliberately ::Int to test type-changes
    for x in list
        s += x
    end
    return s
end
function summer_iterate(list)
    # same as above, but with an explicit call it `iterate` to ensure our handling
    # of implicit `iterate` doesn't mess up explicit `iterate`
    s = 0
    ret = iterate(list)
    while ret !== nothing
        x, state = ret
        s += x
        ret = iterate(list, state)
    end
    return s
end

zerowhere(::AbstractArray{T}) where T<:Real = zero(T)
vaparam(a::AbstractArray{T,N}, I::NTuple{N,Any}) where {T,N} = N
@inline function myplustv(x::T, y::Integer) where {T<:AbstractChar}  # vendored copy of +(::T, ::Integer) where T<:AbstractChar
    if x isa Char
        u = Int32((bitcast(UInt32, x) >> 24) % Int8)
        if u >= 0 # inline the runtime fast path
            z = u + y
            return 0 <= z < 0x80 ? bitcast(Char, (z % UInt32) << 24) : Char(UInt32(z))
        end
    end
    return T(Int32(x) + Int32(y))
end

function val(::Val{N}) where N
    if N == 2
        return 4
    else
        return 3
    end
end

unnamedargs(::Type{<:AbstractMatrix{T}}, ::Type{Int}, c=1; a=1) where T<:Real = a
unnamedargs2(::Type{Matrix}, op::Symbol; padding::Bool=false) = padding
cb(a, i) = checkbounds(Bool, a, i)

add2(x) = x[1] + x[2]

myabs(x) = x < 0 ? -x : x

likevect(X::T...) where {T} = T[ X[i] for i = 1:length(X) ]
cbva(a, i...) = checkbounds(Bool, a, i...)
anykwargs(; kwargs...) = println(kwargs...)
splats(x, y) = vcat(x..., y...)

myoftype(ref, val) = typeof(ref)(val)

defaultarg(x, y=2) = x + y
hasdefaulttypearg(::Type{T}=Rational{Int}) where T = zero(T)

charset1 = 'a':'z'
getchar1(idx) = charset1[idx]
const charset2 = 'a':'z'
getchar2(idx) = charset2[idx]

# unused statements
function mycheckbounds(A, i)
    checkbounds(Bool, A, i) || Base.throw_boundserror(A, i)
    return nothing
end

# Globals & scoped assignment
myglobal = nothing
function setglobal(val)
    global myglobal = val
end

# Implementation of a struct & interface
struct DefaultArray{T,N,A<:AbstractArray{T,N}} <: AbstractArray{T,N}
    parentarray::A
    defaultvalue::T
end
function DefaultArray{T}(parentarray::AbstractArray, defaultvalue) where T
    p = convert(AbstractArray{T}, parentarray)
    return DefaultArray{T, ndims(parentarray), typeof(p)}(p, convert(T, defaultvalue))
end
function DefaultArray(parentarray::AbstractArray, defaultvalue)
    T = promote_type(eltype(parentarray), typeof(defaultvalue))
    return DefaultArray{T}(parentarray, defaultvalue)
end
Base.getindex(a::DefaultArray{T,N}, i::Vararg{Int,N}) where {T,N} = checkbounds(Bool, a, i...) ? a.parentarray[i...] : a.defaultvalue
Base.size(a::DefaultArray) = size(a.parentarray)

# macros in the function body (which introduce novel symbols)
function hasmacro(t, x)
    rand()
    convert(Base.@default_eltype(t), x)
end

# This has a TypedSlot in an indexed_iterate call
function typeof_first_item(g::Base.Generator)
    y = iterate(g)
    y === nothing && return Nothing
    val, s = y
    return typeof(val)
end

# Generators (issue #368)
function boxedgenerator368(x)
    if x > 1
        y = 3
    else
        y = 4
    end
    [y + i for i in 1:4]
end
nestedgenerators(j, k) = (a^2 for a = 1:j for _ = 1:k)
nestedgenerators(j) = (a^2 for a = 1:j for _ = 1:j)
nestedexplicit(k) = [Base.Generator(identity, 1:3) for _ = 1:k]

# Broadcasting
fbroadcast(list) = sum(sin.(list))
fbroadcast_explicit(list) = sum(Base.materialize(Base.broadcasted(sin, list)))
fbroadcast2(list) = join("; value: " .* string.(list))   # double-broadcasted (.* and string.)
struct B415
    y::Float64
    B415(y) = new(y)
end
function bcast415(b, x)
    return (x .+ 2.0) .+ b.y
end

# Lowered to `firstindex`
myunique(r::AbstractRange) = allunique(r) ? r : oftype(r, r[begin:begin])

# Argument annotations
nospec(@nospecialize(x)) = 2x
nospec2(@nospecialize(x::AbstractVecOrMat)) = first(x)

# Return-type annotation
withrt(io::IO)::Bool = eof(io)
function mytimes(x::Bool, y::T)::promote_type(Bool,T) where T<:AbstractFloat
    return ifelse(x, y, copysign(zero(y), y))
end

# Operators
struct MyInt x::Int end
Base.:+(a::MyInt, b::MyInt) = MyInt(a.x + b.x)
import Base: -
-(a::MyInt, b::MyInt) = MyInt(a.x - b.x)

# Nested tuple destructuring (issue #381)
struct Foo381
    a
    b
end
function bar381(foo)
    a, (b1, b2) = foo.a, foo.b
    return b1
end
extrema2((min1, max1), (min2, max2)) = (min(min1, min2), max(max1, max2))

# Generated functions (issue #385)
function _generate_body385(N::Int)
    quote
        D = eachindex(dest)
        Dy = iterate(D)
        (idx, state) = Dy
        return ndims(D)
    end
end
@eval generated385(dest::AbstractVector) = $(_generate_body385(1))

# quoted `=`
isexpreq(ex::Expr) = ex.head ∈ (:(=), :(.=))

# Computing the number of args in the signature (issue #397)
f397(x::SubArray{T, N, P, I, L}) where {T,N,P,I,L} = isempty(x)

# global
let
    global in_let(x) = x^2
end

# Scoped + interpolated function names (taken from `eltype(::SparseArrays.ReadOnly)`)
struct ReadOnly end
fname = :eltype
@eval Base.@propagate_inbounds @inline Base.$fname(::ReadOnly) = Int

# Issue #426
const T426 = Dict{Type{<:Dates.Period}, Bool}(
    Dates.Year => true,
    Dates.Month => false,
)

# Issue #458
f458() = return

function fVSCode(x)
    y = x + 1
    z = 2 * y
    return z + a
end

end
