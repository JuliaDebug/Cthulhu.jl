# TypedSyntax

This package aims to map types, as determined via type-inference, back to the source code as written by the developer. It can be used to understand program behavior and identify causes of "type instability" (inference failures) without the need to read [intermediate representations](https://docs.julialang.org/en/v1/devdocs/ast/) of Julia code.

This package is built on [JuliaSyntax](https://github.com/JuliaLang/JuliaSyntax.jl) and extends it by attaching type annotations to the nodes of its syntax trees. Here's a demo:

```julia
julia> using TypedSyntax

julia> f(x, y, z) = x + y * z;

julia> node = TypedSyntaxNode(f, (Float64, Int, Float32))
line:col│ tree                                   │ type
   1:1  │[=]                                     │Float64
   1:1  │  [call]
   1:1  │    f
   1:3  │    x                                   │Float64
   1:6  │    y                                   │Int64
   1:9  │    z                                   │Float32
   1:13 │  [call-i]                              │Float64
   1:14 │    x                                   │Float64
   1:16 │    +
   1:17 │    [call-i]                            │Float32
   1:18 │      y                                 │Int64
   1:20 │      *
   1:22 │      z                                 │Float32
```

The right hand column is the new information added by `TypedSyntaxNode`, indicating the type assigned to each variable or function call.

You can also display this in a form closer to the original source code, but with type-annotations:

```julia
julia> printstyled(stdout, node; hide_type_stable=false)
f(x::Float64, y::Int64, z::Float32)::Float64 = (x::Float64 + (y::Int64 * z::Float32)::Float32)::Float64
```

`hide_type_stable=true` (which is the default) will suppress printing of concrete types, so you need to set it to `false` if you want to see all the types.

The default is aimed at identifying sources of "type instability" (poor inferrability):

```julia
julia> printstyled(stdout, TypedSyntaxNode(f, (Float64, Int, Real)))
```

which produces

<code>f(x, y, z::<b>Real</b>)::<b>Any</b> = (x + (y * z::<b>Real</b>)::<b>Any</b>)::<b>Any</b></code>

The boldfaced text above is typically printed in color in the REPL:

- red indicates non-concrete types
- yellow indicates a "small union" of concrete types. These usually pose no issues, unless there are too many combinations of such unions.

Printing with color can be suppressed with the keyword argument `iswarn=false`.

## Caveats

TypedSyntax aims for accuracy, but there are a number of factors that pose challenges.
First, anonymous and internal functions appear as part of the source text, but internally Julia handles these as separate type-inferred methods, and these are hidden from the annotator.
Therefore, in

```julia
julia> sumfirst(c) = sum(x -> first(x), c);    # better to use `sum(first, c)` but this is just an illustration

julia> printstyled(stdout, TypedSyntaxNode(sumfirst, (Vector{Any},)))
sumfirst(c)::Any = sum(x -> first(x), c)::Any
```

`x` and `first(x)` both have type `Any`, but they are not annotated as such because they are hidden inside the anonymous function.

Second, this package works by attempting to "reconstruct history": starting from the type-inferred code, it tries to map calls back to the source. It would be much safer to instead keep track of the source during inference, but at present this is not possible (see [this Julia issue](https://github.com/JuliaLang/julia/issues/31162)). There are cases where this mapping fails: for example, with

```julia
julia> function summer(list)
           s = 0
           for x in list
               s += x
           end
           return s
       end;
```
the type-inferred code is (on Julia 1.9)
```
CodeInfo(
    @ REPL[3]:2 within `summer`
1 ─       (s = 0)::Core.Const(0)
│   @ REPL[3]:3 within `summer`
│   %2  = list::Vector{Float64}
│         (@_3 = Base.iterate(%2))::Union{Nothing, Tuple{Float64, Int64}}
│   %4  = (@_3 === nothing)::Bool
│   %5  = Base.not_int(%4)::Bool
└──       goto #4 if not %5
2 ┄ %7  = @_3::Tuple{Float64, Int64}
│         (x = Core.getfield(%7, 1))::Float64
│   %9  = Core.getfield(%7, 2)::Int64
│   @ REPL[3]:4 within `summer`
│         (s = s + x)::Float64
│   @ REPL[3]:5 within `summer`
│         (@_3 = Base.iterate(%2, %9))::Union{Nothing, Tuple{Float64, Int64}}
│   %12 = (@_3 === nothing)::Bool
│   %13 = Base.not_int(%12)::Bool
└──       goto #4 if not %13
3 ─       goto #2
    @ REPL[3]:6 within `summer`
4 ┄       return s
) => Union{Float64, Int64}
```
and very few calls here map to the source:
```julia
16-element Vector{Vector{Union{JuliaSyntax.TreeNode{JuliaSyntax.SyntaxData}, JuliaSyntax.TreeNode{TypedSyntax.TypedSyntaxData}}}}:
 %1: []
 %2: [list]
 %3: [(= x list)]
 %4: []
 %5: []
 %6: []
 %7: []
 %8: []
 %9: []
%10: [(+= s x)]
%11: []
%12: []
%13: []
%14: []
%15: []
%16: [s]
```

This is because lowering changes the implementation so significantly that there are few calls that relate directly to the source.
Nevertheless many statements in the source can be annotated:

```julia
julia> tsn
line:col│ tree                                   │ type
   1:1  │[function]                              │Union{Float64, Int64}
   1:10 │  [call]
   1:10 │    summer
   1:17 │    list                                │Vector{Float64}
   1:22 │  [block]
   2:5  │    [=]
   2:5  │      s
   2:9  │      0
   3:5  │    [for]
   3:8  │      [=]                               │Union{Nothing, Tuple{Float64, Int64}}
   3:9  │        x
   3:14 │        list                            │Vector{Float64}
   3:18 │      [block]
   4:9  │        [+=]                            │Float64
   4:9  │          s                             │Float64
   4:14 │          x                             │Float64
   6:5  │    [return]                            │Union{Float64, Int64}
   6:12 │      s                                 │Union{Float64, Int64}
```
This is largely because just the named-variables provide considerable information.
