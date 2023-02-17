# TypedSyntax

This package aims to map types, as determined via type-inference, back to the source code as written by the developer. It can be used to understand program behavior and identify causes of "type instability" (inference failures) without the need to read Julia's [intermediate representations](https://docs.julialang.org/en/v1/devdocs/ast/) of code.

This package is built on [JuliaSyntax](https://github.com/JuliaLang/JuliaSyntax.jl) and extends it by attaching type annotations to the nodes of its syntax trees. Here's a demo:

```julia
julia> using TypedSyntax

julia> f(x, y, z) = x + y * z;

julia> node = TypedSyntaxNode(f, (Float64, Int, Float32))
line:col│ byte_range  │ tree                                   │ type or call idxs
   1:1  │     1:22    │[=]
   1:1  │     1:10    │  [call]                                │Int64[]
   1:1  │     1:1     │    f                                   │TypedSyntax.NotFound
   1:3  │     3:3     │    x                                   │Float64
   1:6  │     6:6     │    y                                   │Int64
   1:9  │     9:9     │    z                                   │Float32
   1:13 │    13:22    │  [call-i]                              │Float64
   1:14 │    14:14    │    x                                   │Float64
   1:16 │    16:16    │    +
   1:17 │    17:22    │    [call-i]                            │Float32
   1:18 │    18:18    │      y                                 │Int64
   1:20 │    20:20    │      *
   1:22 │    22:22    │      z                                 │Float32
```

The right hand column is the new information added by `TypedSyntaxNode`: each is either a type or a list of integers (indicating a failure to map to a unique type in the type-inferred code).

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

Second, because not all expressions can be matched, there are cases where some of the matches are ambiguous.
Consider the following example:

```
julia> firstfirst(c) = map(x -> first(x), first(c));

julia> TypedSyntaxNode(firstfirst, (Vector{Any},))
line:col│ byte_range  │ tree                                   │ type or call idxs
   1:1  │     1:44    │[=]
   1:1  │     1:13    │  [call]                                │Int64[]
   1:1  │     1:10    │    firstfirst                          │TypedSyntax.NotFound
   1:12 │    12:12    │    c                                   │Vector{Any}
   1:17 │    17:44    │  [call]                                │Any
   1:17 │    17:19    │    map                                 │TypedSyntax.NotFound
   1:21 │    21:33    │    [->]
   1:21 │    21:21    │      x                                 │TypedSyntax.NotFound
   1:26 │    26:33    │      [call]                            │[3]
   1:26 │    26:30    │        first                           │TypedSyntax.NotFound
   1:32 │    32:32    │        x                               │TypedSyntax.NotFound
   1:36 │    36:43    │    [call]                              │[3]
   1:36 │    36:40    │      first                             │TypedSyntax.NotFound
   1:42 │    42:42    │      c                                 │Vector{Any}
```

Note that the two `[call]` expressions involving `first` are marked with "type" `[3]`.
Since this vector has only one element, it means that only one type-inferred call to `first` could be found.
However, there were two source statements "competing" to be assigned to it.
Since it could not uniquely resolve the caller, these are marked in yellow with `::NF` (for "not found"):

```julia
julia> printstyled(stdout, TypedSyntaxNode(firstfirst, (Vector{Any},)))
firstfirst(c)::Any = map(x -> first(x)::NF, first(c)::NF)::Any
```
