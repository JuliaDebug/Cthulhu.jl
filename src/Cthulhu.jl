module Cthulhu

export @descend, @descend_code_typed, @descend_code_warntype,
    descend, descend_code_typed, descend_code_warntype, ascend,
    AbstractProvider,
    get_module_for_compiler_integration

const CC = Base.Compiler
const IRShow = Base.IRShow

using Accessors
using CodeTracking: CodeTracking, definition, whereis, maybe_fix_path
using InteractiveUtils
using InteractiveUtils: is_expected_union
using UUIDs
using REPL: REPL, AbstractTerminal
using JuliaSyntax: JuliaSyntax, children
using TypedSyntax
import WidthLimitedIO: TextWidthLimiter
using Preferences

using Core.IR
using Base: default_tt, unwrap_unionall, mapany

global CompilerExt::Union{Nothing, Module}

function get_module_for_compiler_integration(; use_compiler_stdlib::Bool = true)
    !use_compiler_stdlib && return @__MODULE__
    isdefined(@__MODULE__, :CompilerExt) || error("The Cthulhu -> Compiler extension must be loaded first")
    return something(CompilerExt, @__MODULE__)
end

cached_exception_type(code::CodeInstance) = code.exctype
get_mi(ci::CodeInstance) = CC.get_ci_mi(ci)
get_mi(mi::MethodInstance) = mi

include("config.jl")
include("preferences.jl")
__init__() = read_config!()

include("interface.jl")
include("callsite.jl")
include("state.jl")
include("ui.jl")
include("descend.jl")
include("ascend.jl")
include("bookmark.jl")
include("backedges.jl")
include("testing.jl")

include("CthulhuCompiler.jl")

"""
    @descend

Evaluates the arguments to the function or macro call, determines their
types, and calls [`descend`](@ref) on the resulting expression.
See [`Cthulhu.CONFIG`](@ref) for options and their defaults.

# Examples
```julia
julia> @descend sin(1)
[...]

julia> @descend view=:typed iswarn=true optimize=false foo() # equivalent to `@descend_warntype`
[...]

julia> @descend view=:typed iswarn=false foo() # equivalent to `@descend_code_typed`
[...]

julia> @descend interp=SomeInterpreter() foo() # use a custom `Compiler.AbstractInterpreter`
[...]

julia> @descend provider=SomeProvider() foo() # use a custom `AbstractProvider`, see the docs for more details
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

This macro is equivalent to `@descend` with the following options set (unless provided):
- `view = :typed`
- `iswarn = false`

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

This macro is equivalent to `@descend` with the following options set (unless provided):
- `view = :typed`
- `iswarn = true`
- `optimize = false`

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
    descend(Cthulhu.BOOKMARKS[i]; kwargs...)
    descend(mi::MethodInstance; kwargs...)

Given a function and a tuple-type, interactively explore the source code of functions
annotated with inferred types by descending into `invoke` statements. Type enter to select a callsite
to descend into, select `↩` or press backspace to ascend, and press `q` or `ctrl-c` to quit.
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
    descend_with_error_handling(args...; kwargs...)
    return nothing
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

get_specialization(@nospecialize(f), @nospecialize(tt=default_tt(f))) =
    get_specialization(Base.signature_type(f, tt))
get_specialization(@nospecialize tt::Type{<:Tuple}) =
    specialize_method(Base._which(tt))

end # module Cthulhu
