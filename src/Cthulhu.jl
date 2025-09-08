module Cthulhu

export @descend, @descend_code_typed, @descend_code_warntype,
    descend, descend_code_typed, descend_code_warntype, ascend,
    AbstractProvider

const CC = Base.Compiler
const IRShow = Base.IRShow

const CTHULHU_MODULE = Ref{Module}(@__MODULE__)

function resolve_module(Compiler::Module)
    Compiler === Base.Compiler && return @__MODULE__
    Compiler === CTHULHU_MODULE[].Compiler && return CTHULHU_MODULE[]
    return resolve_module()
end
resolve_module() = CTHULHU_MODULE[]
function resolve_module(@nospecialize(::T)) where {T}
    mod = parentmodule(T)
    nameof(mod) === :Compiler && return resolve_module(mod)
    return resolve_module()
end

__init__() = read_config!()

include("CthulhuBase.jl")
include("backedges.jl")
include("testing.jl")

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
function descend(@nospecialize(args...); interp=nothing,
                                         provider=nothing,
                                         @nospecialize(kwargs...))
    if provider !== nothing
        mod = resolve_module(provider)
        return mod.descend_impl(args...; provider, kwargs...)
    elseif interp !== nothing
        mod = resolve_module(interp)
        return mod.descend_impl(args...; interp, kwargs...)
    else
        mod = resolve_module()
        return mod.descend_impl(args...; kwargs...)
    end
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
descend_code_typed(@nospecialize(args...); view = :typed, iswarn = false, kwargs...) =
    descend(args...; view, iswarn, kwargs...)

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
descend_code_warntype(@nospecialize(args...); view = :typed, iswarn = true, optimize = false, kwargs...) =
    descend(args...; view, iswarn, optimize, kwargs...)

"""
    ascend(mi::MethodInstance; kwargs...)
    ascend(bt; kwargs...)

Follow a chain of calls (either through a backtrace `bt` or the backedges of a `MethodInstance` `mi`),
with the option to `descend` into intermediate calls.

Keyword arguments `pagesize, dynamic, maxsize` are passed to `Cthulhu.FoldingTrees.TreeMenu`.
Any remaining `kwargs` are passed to [`descend`](@ref).
"""
function ascend(@nospecialize(args...); kwargs...)
    CTHULHU_MODULE[].ascend_impl(args...; kwargs...)
end

using PrecompileTools
@setup_workload begin
    try
        @compile_workload begin
            # terminal = Testing.VirtualTerminal()
            # task = @async @descend terminal=terminal.tty gcd(1, 2)
            # write(terminal, 'q')
            # wait(task)
            # finalize(terminal)
        end
    catch err
        @error "Errorred while running the precompile workload, the package may or may not work but latency will be long" exeption=(err,catch_backtrace())
    end
end

end # module Cthulhu
