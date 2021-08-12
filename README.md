# Cthulhu.jl
*The slow descent into madness*

[![Build Status](https://github.com/JuliaDebug/Cthulhu.jl/workflows/CI/badge.svg)](https://github.com/JuliaDebug/Cthulhu.jl/actions?query=workflow%3A%22CI%22+branch%3Amaster)
[![Codecov](https://codecov.io/github/JuliaDebug/Cthulhu.jl/coverage.svg)](https://codecov.io/gh/JuliaDebug/Cthulhu.jl)

:warning: The latest stable version is only compatible with Julia v1.7 and higher.

Cthulhu can help you debug type inference issues by recursively showing the
`code_typed` output until you find the exact point where inference gave up,
messed up, or did something unexpected. Using the Cthulhu interface you can
debug type inference problems faster.

Looking at type-inferred code can be a bit daunting initially, but you grow more
comfortable with practice. Consider starting with a [tutorial on "lowered" representation](https://juliadebug.github.io/JuliaInterpreter.jl/stable/ast/),
which introduces most of the new concepts. Type-inferrred code differs mostly
by having additional type annotation and (depending on whether you're looking
at optimized or non-optimized code) may incorporate inlining and other fairly
significant transformations of the original code as written by the programmer.

Cthulhu's main tool, `descend`, can be invoked like this:

```julia
descend(f, tt)     # function and argument types
@descend f(args)   # normal call
```

`descend` allows you to interactively explore the output of
`code_typed` by descending into `invoke` and `call` statements. (`invoke`
statements correspond to static dispatch, whereas `call` statements correspond
to dynamic dispatch.) Press enter to select an `invoke` or `call` to descend
into, select ↩  to ascend, and press q or control-c to quit.

### JuliaCon 2019 Talk and Demo
[Watch on YouTube](https://www.youtube.com/watch?v=qf9oA09wxXY)  
[![Click to watch video](https://img.youtube.com/vi/qf9oA09wxXY/0.jpg)](https://www.youtube.com/watch?v=qf9oA09wxXY)

The version of Cthulhu in the demo is a little outdated, without the newest features, but largely it has not changed too much.

## Usage: descend

```julia
function foo()
    T = rand() > 0.5 ? Int64 : Float64
    sum(rand(T, 100))
end

descend(foo, Tuple{})
@descend foo()
```

## Methods: descend

- `@descend_code_typed`
- `descend_code_typed`
- `@descend_code_warntype`
- `descend_code_warntype`
- `@descend`: Shortcut for `@descend_code_typed`
- `descend`: Shortcut for `descend_code_typed`

## Usage: ascend

Cthulhu also provides the "upwards-looking" `ascend`. While `descend` allows
you to explore a call tree starting from the outermost caller, `ascend`
allows you to explore a call chain or tree starting from the innermost
callee. Its primary purpose is to support analysis of invalidation and inference
triggers in conjunction with [SnoopCompile](https://github.com/timholy/SnoopCompile.jl),
but you can use it as a standalone tool.
There is a [video using ascend to fix invalidations](https://www.youtube.com/watch?v=7VbXbI6OmYo),
where the part on `ascend` starts at minute 4:55.

For example, you can use it to examine all the inferred callers of a method instance:

```julia
julia> m = which(length, (Set{Symbol},))
length(s::Set) in Base at set.jl:55

julia> mi = m.specializations[1]
MethodInstance for length(::Set{Symbol})

julia> ascend(mi)
Choose a call for analysis (q to quit):
 >   length(::Set{Symbol})
       union!(::Set{Symbol}, ::Vector{Symbol})
         Set{Symbol}(::Vector{Symbol})
         intersect!(::Set{Union{Int64, Symbol}}, ::Vector{Symbol})
           _shrink(::typeof(intersect!), ::Vector{Union{Int64, Symbol}}, ::Tuple{Vector{Symbol}})
             intersect(::Vector{Union{Int64, Symbol}}, ::Vector{Symbol})
       union!(::Set{Symbol}, ::Set{Symbol})
         union!(::Set{Symbol}, ::Set{Symbol}, ::Set{Symbol})
           union(::Set{Symbol}, ::Set{Symbol})
```
You use the up/down arrows to navigate this menu, enter to select a call to `descend` into,
and your space bar to toggle branch-folding.

It also works on stacktraces:

```julia
julia> bt = try
           [sqrt(x) for x in [1, -1]]
       catch
           catch_backtrace()
       end;

julia> ascend(bt)
Choose a call for analysis (q to quit):
 >   throw_complex_domainerror(::Symbol, ::Float64) at ./math.jl:33
       sqrt at ./math.jl:582 => sqrt at ./math.jl:608 => iterate at ./generator.jl:47 => collect_to! at ./array.jl:710 => collect_to_with_first!(::Vector{Float64}, ::Float64, ::Base.Generator{Vector{Int64}, typeof(sqrt)}, ::Int64) at ./array.jl:688
         collect(::Base.Generator{Vector{Int64}, typeof(sqrt)}) at ./array.jl:669
           eval(::Module, ::Any) at ./boot.jl:360
             eval_user_input(::Any, ::REPL.REPLBackend) at /home/tim/src/julia-master/usr/share/julia/stdlib/v1.6/REPL/src/REPL.jl:139
...
```

The calls that appear on the same line separated by `=>` represent inlined methods; when you select such a line,
you enter at the final (topmost) call on that line.

By default,
- `descend` views optimized code without "warn" coloration of types
- `ascend` views non-optimized code with "warn" coloration

You can toggle between these with `o` and `w`.

## Combine static and runtime information

Cthulhu has access only to "static" type information, the same information available to the Julia compiler and type inference.
In some situations, this will lead to incomplete or misleading information about type instabilities.

Take for example: 
```julia
using Infiltrator: @infiltrate
using Cthulhu: @descend
using Base: @noinline # already exported, but be explcit


function foo(n)
    x = n < 2 ? 2 * n : 2.5 * n
    y = n < 4 ? 3 * n : 3.5 * n
    z = n < 5 ? 4 * n : 4.5 * n
    # on Julia v1.6, there is no union splitting for this number of cases.
    bar(x, y, z)
end

@noinline function bar(x, y, z)
    string(x + y + z)
end
```

Then invoke:

```julia
Cthulhu.@descend foo(5)
```

Now, descend:

```
%22  = call bar(::Union{Float64, Int64},::Union{Float64, Int64},::Union{Float64, Int64})::String
```

which shows (after typing `w`)

```
│ ─ %-1  = invoke bar(::Union{Float64, Int64},::Union{Float64, Int64},::Union{Float64, Int64})::String
Variables
  #self#::Core.Const(bar)
  x::Union{Float64, Int64}
  y::Union{Float64, Int64}
  z::Union{Float64, Int64}
[...]
```

The text of `Union{Float64, Int64}`  will be in red, but it is likely that `bar` will be called via dynamic dispatch.
`bar` will be a ["function barrier"](https://docs.julialang.org/en/v1/manual/performance-tips/#kernel-functions), and the types will be fully inferred within `bar`.

To give Cthulhu more complete type information, we have to actually run some Julia code. There are many ways to do this. In this example, we use [`Infiltrator.jl`](https://github.com/JuliaDebug/Infiltrator.jl).

Add an `@infiltrate`:

```julia
function foo(n)
    x = n < 2 ? 2 * n : 2.5 * n
    y = n < 4 ? 3 * n : 3.5 * n
    z = n < 5 ? 4 * n : 4.5 * n
    # on Julia v1.6, there is no union splitting for this number of cases.
    @infiltrate
    bar(x, y, z)
end

@noinline function bar(x, y, z)
    string(x + y + z)
end
```

Now invoke `foo` to get REPL in the scope just before `bar` gets called:

```julia
julia> foo(4)
Infiltrating foo(n::Int64) at ex.jl:10:

infil> 
```

Enter `@descend bar(x, y, z)` and type `w`:

```
infil> @descend bar(x, y, z)

│ ─ %-1  = invoke bar(::Float64,::Float64,::Int64)::String
Variables
  #self#::Core.Const(bar)
  x::Float64
  y::Float64
  z::Int64
[...]
```

You can see that, for `foo(4)`, the types within `bar` are fully inferred.

