# Cthulhu.jl
*The slow descent into madness*

[![Build Status](https://github.com/JuliaDebug/Cthulhu.jl/workflows/CI/badge.svg)](https://github.com/JuliaDebug/Cthulhu.jl/actions?query=workflow%3A%22CI%22+branch%3Amaster)
[![Codecov](https://codecov.io/github/JuliaDebug/Cthulhu.jl/coverage.svg)](https://codecov.io/gh/JuliaDebug/Cthulhu.jl)

Cthulhu can help you debug type inference issues by recursively showing the
`code_typed` output until you find the exact point where inference gave up,
messed up, or did something unexpected. Using the Cthulhu interface you can
debug type inference problems faster.

```julia
descend(f, tt)
@descend f()
```

Given a function and a tuple-type, interactively explore the output of
`code_typed` by descending into `invoke` and `call` statements. (`invoke`
statements correspond to static dispatch, whereas `call` statements correspond
to dynamic dispatch.) Press enter to select an `invoke` or `call` to descend
into, select ↩  to ascend, and press q or control-c to quit.

### JuliaCon 2019 Talk and Demo
[Watch on YouTube](https://www.youtube.com/watch?v=qf9oA09wxXY)  
[![Click to watch video](https://img.youtube.com/vi/qf9oA09wxXY/0.jpg)](https://www.youtube.com/watch?v=qf9oA09wxXY)

The version of Cthulu in the demo is a little outdated, without the newest features, but largely it has not changed too much.

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
