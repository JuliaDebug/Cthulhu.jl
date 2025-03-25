module Cthulhu

export @descend, @descend_code_typed, @descend_code_warntype,
    descend, descend_code_typed, descend_code_warntype, ascend

@static if VERSION ≥ v"1.12.0-DEV.1581"
    const CC = Base.Compiler
    const IRShow = Base.IRShow
else
    const CC = Core.Compiler
    const IRShow = Base.IRShow
end

const CTHULHU_MODULE = Ref{Module}(@__MODULE__)

__init__() = read_config!(CONFIG)

include("CthulhuBase.jl")

"""
    @descend

Evaluates the arguments to the function or macro call, determines their
types, and calls [`descend`](@ref) on the resulting expression.
See [`Cthulhu.CONFIG`](@ref) for options and their defaults.

# Examples
```julia
julia> @descend sin(1)
[...]

julia> @descend iswarn=false foo()
[...]
```
"""
macro descend(ex0...)
    InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :descend, ex0)
end

"""
    @descend_code_typed

Evaluates the arguments to the function or macro call, determines their
types, and calls [`descend_code_typed`](@ref) on the resulting expression.
See [`Cthulhu.CONFIG`](@ref) for options and their defaults.

# Examples
```julia
julia> @descend_code_typed sin(1)
[...]

julia> @descend_code_typed optimize=false sin(1)
[...]
```
"""
macro descend_code_typed(ex0...)
    InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :descend_code_typed, ex0)
end

"""
    @descend_code_warntype

Evaluates the arguments to the function or macro call, determines their
types, and calls [`descend_code_warntype`](@ref) on the resulting expression.
See [`Cthulhu.CONFIG`](@ref) for options and their defaults.

# Examples
```julia
julia> function foo()
           T = rand() > 0.5 ? Int64 : Float64
           sin(rand(T))
       end
foo (generic function with 1 method)

julia> @descend_code_warntype foo()
[...]

julia> @descend_code_warntype hide_type_stable=true foo()
[...]
```
"""
macro descend_code_warntype(ex0...)
    InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :descend_code_warntype, ex0)
end

"""
    descend(f, argtypes=Tuple{...}; kwargs...)
    descend(tt::Type{<:Tuple}; kwargs...)
    descend(Cthulhu.BOOKMARKS[i])
    descend(mi::MethodInstance; kwargs...)

Given a function and a tuple-type, interactively explore the source code of functions
annotated with inferred types by descending into `invoke` statements. Type enter to select an
`invoke` to descend into, select `↩` to ascend, and press `q` or `control-c` to quit.
See [`Cthulhu.CONFIG`](@ref) for `kwargs` and their defaults.

# Usage:
```julia
julia> descend(sin, (Int,))
[...]

julia> descend(sin, Tuple{Int})
[...]

julia> descend(Tuple{typeof(sin), Int})
[...]

julia> descend() do
           T = rand() > 0.5 ? Int64 : Float64
           sin(rand(T))
       end
[...]
```
"""
function descend(@nospecialize(args...); @nospecialize(kwargs...))
    CTHULHU_MODULE[].descend_impl(args...; kwargs...)
end

"""
    descend_code_typed(f, argtypes=Tuple{...}; kwargs...)
    descend_code_typed(tt::Type{<:Tuple}; kwargs...)
    descend_code_typed(Cthulhu.BOOKMARKS[i]; kwargs...)
    descend_code_typed(mi::MethodInstance; kwargs...)

Given a function and a tuple-type, interactively explore the output of
`code_typed` by descending into `invoke` statements. Type enter to select an
`invoke` to descend into, select `↩`  to ascend, and press `q` or `control-c` to quit.
See [`Cthulhu.CONFIG`](@ref) for `kwargs` and their defaults.

# Usage:
```julia
julia> descend_code_typed(sin, (Int,))
[...]

julia> descend_code_typed(sin, Tuple{Int})
[...]

julia> descend_code_typed(Tuple{typeof(sin), Int})
[...]

julia> descend_code_typed() do
           T = rand() > 0.5 ? Int64 : Float64
           sin(rand(T))
       end
[...]
```
"""
function descend_code_typed(@nospecialize(args...); kwargs...)
    CTHULHU_MODULE[].descend_code_typed(args...; kwargs...)
end

"""
    descend_code_warntype(f, argtypes=Tuple{...}; kwargs...)
    descend_code_warntype(tt::Type{<:Tuple}; kwargs...)
    descend_code_warntype(Cthulhu.BOOKMARKS[i])
    descend_code_warntype(mi::MethodInstance; kwargs...)

Given a function and a tuple-type, interactively explore the output of
`code_warntype` by descending into `invoke` statements. Type enter to select an
`invoke` to descend into, select `↩` to ascend, and press `q` or `control-c` to quit.
See [`Cthulhu.CONFIG`](@ref) for `kwargs` and their defaults.

# Usage:
```julia
julia> descend_code_warntype(sin, (Int,))
[...]

julia> descend_code_warntype(sin, Tuple{Int})
[...]

julia> descend_code_warntype(Tuple{typeof(sin), Int})
[...]

julia> descend_code_warntype() do
           T = rand() > 0.5 ? Int64 : Float64
           sin(rand(T))
       end
[...]
```
"""
function descend_code_warntype(@nospecialize(args...); kwargs...)
    CTHULHU_MODULE[].descend_code_warntype_impl(args...; kwargs...)
end

"""
    ascend(mi::MethodInstance; kwargs...)
    ascend(bt; kwargs...)

Follow a chain of calls (either through a backtrace `bt` or the backedges of a `MethodInstance` `mi`),
with the option to `descend` into intermediate calls. `kwargs` are passed to [`descend`](@ref).
"""
function ascend(@nospecialize(args...); kwargs...)
    CTHULHU_MODULE[].ascend_impl(args...; kwargs...)
end

using PrecompileTools
@setup_workload begin
    try
        input = Base.link_pipe!(Pipe(), reader_supports_async=true, writer_supports_async=true)
        term = REPL.Terminals.TTYTerminal("dumb", input.out, devnull, devnull)
        write(input.in, 'q')

        @compile_workload descend(gcd, (Int, Int); terminal=term)

        # @static if VERSION ≥ v"1.12.0-DEV.1581"
        #     using Compiler
        #     @compile_workload descend(gcd, (Int, Int); terminal=term)
        # end

        # declare we are done with streams
        close(input.in)
    catch err
        @error "Errorred while running the precompile workload, the package may or may not work but latency will be long" exeption=(err,catch_backtrace())
    end
end

end # module Cthulhu
