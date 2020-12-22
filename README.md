# Cthulhu.jl
*The slow descent into madness*

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

## Usage

```julia
function foo()
    T = rand() > 0.5 ? Int64 : Float64
    sum(rand(T, 100))
end

descend(foo, Tuple{})
@descend foo()
```

Cthulhu also provides the "upwards-looking" `ascend`. Its primary purpose is to
support analysis of invalidation and inference triggers in conjunction with
[SnoopCompile](https://github.com/timholy/SnoopCompile.jl), but you can use
it as a standalone tool to examine all the inferred callers of a method instance:

```julia
julia> m = @which length("Hello")
length(s::String) in Base at strings/string.jl:272

julia> mi = m.specializations[1]
MethodInstance for length(::String)

julia> ascend(mi)
Choose a call for analysis (q to quit):
 >   length(::String)
       rpad(::String, ::Int64, ::String)
         (::Main.anonymous.var"#1#2")(::Module, ::Float64)
         (::Main.anonymous.var"#1#2")(::Symbol, ::Float64)
         (::Main.anonymous.var"#1#2")(::String, ::Float64)
         (::Pkg.Operations.var"#59#63"{Int64, Bool, Pkg.MiniProgressBars.MiniProgressBar, Bool, Pkg.Types.PackageSpec})(::IOContext{IOBuffer})
         #download_source#55(::Bool, ::typeof(Pkg.Operations.download_source), ::Pkg.Types.Context, ::Vector{Pkg.Types.PackageSpec}, ::Dict{Base.UUID, Vector{String}})
           (::Pkg.Operations.var"#download_source##kw")(::NamedTuple{(:readonly,), Tuple{Bool}}, ::typeof(Pkg.Operations.download_source), ::Pkg.Types.Context, ::Vector{Pkg.Types.PackageSpec}, ::Dict{Bas
             #download_source#54(::Bool, ::typeof(Pkg.Operations.download_source), ::Pkg.Types.Context, ::Vector{Pkg.Types.PackageSpec})
v              download_source(::Pkg.Types.Context, ::Vector{Pkg.Types.PackageSpec})
```

It also works on stacktraces:

```julia
julia> bt = try
           sqrt.([1.0, -1.0])
       catch
           catch_backtrace()
       end;

julia> ascend(bt)
Choose a call for analysis (q to quit):
 >   throw_complex_domainerror(::Symbol, ::Float64)
       sqrt at ./math.jl:582 => _broadcast_getindex_evalf at ./broadcast.jl:648 => _broadcast_getindex at ./broadcast.jl:621 => getindex at ./broadcast.jl:575 => macro expansion at ./broadcast.jl:982 =>
         eval(::Module, ::Any)
           eval_user_input(::Any, ::REPL.REPLBackend)
...
```

## Methods

- `@descend_code_typed`
- `descend_code_typed`
- `@descend_code_warntype`
- `descend_code_warntype`
- `@descend`: Shortcut for `@descend_code_typed`
- `descend`: Shortcut for `descend_code_typed`

- `ascend`
