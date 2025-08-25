"""
    abstract type AbstractProvider end

`AbstractProvider` drives the high-level interface for Cthulhu,
allowing it to *provide* all the data Cthulhu needs for its
display and callsite introspection features.

!!! warning
    This interface is still considered experimental at this time; future changes are to
    be expected, in which case we will do our best to communicate them in CHANGELOG.md.

Cthulhu relies mainly on the following types from Base:
- `MethodInstance`, representing a specialization of a method.
- `CodeInstance`, the high-level result of the Julia compilation pipeline.
- `CodeInfo`, representing Julia code as output by `@code_typed` etc.

And a few from the compiler:
- `Compiler.IRCode`, somewhat similar to `CodeInfo`, but in a different representation.
- `Compiler.Effects`, code metadata used to model part of their semantics and behavior.
- `Compiler.InferenceResult`, holding the results of inference.

Cthulhu is integrated with the Julia compiler, and allows reuse
of its mechanisms to generate these data structures. In particular,
default methods are defined for `AbstractProvider`s that are associated with
a `Compiler.AbstractInterpreter` (the interface type for Compiler, analogous
to `AbstractProvider` for Cthulhu). This allows to reuse some of the commonly
used patterns that are expected with the majority of Compiler integrations.
The association is made by returning an `AbstractInterpreter` with
`get_abstract_interpreter(provider::AbstractProvider)`.

The other way is implemented as well: sometimes, an `AbstractInterpreter` implemented
by a package is central enough that it is part of its API. In this case, this package
may define a constructor `AbstractProvider(interp::AbstractInterpreter)` to automatically
pick the correct provider just by doing `@descend interp=MyInterp() f(x)`.

The interface for `AbstractProvider` requires a few methods to be defined. If it supports
`get_abstract_interpreter(provider::AbstractProvider)::Compiler.AbstractInterpreter`,
then only the following is required:
- `run_type_inference(provider::AbstractProvider, interp::OverlayInterpreter, mi::MethodInstance)`
- `OptimizedSource(provider::AbstractProvider, interp::OverlayInterpreter, ci::CodeInstance)`
- `OptimizedSource(provider::AbstractProvider, interp::OverlayInterpreter, result::InferenceResult)`
- `InferredSource(provider::AbstractProvider, interp::OverlayInterpreter, ci::CodeInstance)`

For an `AbstractProvider` that is not associated with any particular `Compiler.AbstractInterpreter`,
the following methods are required:
- `get_inference_world(provider::AbstractProvider)`, returning the world in which the provider operates.
- `find_method_instance(provider::AbstractProvider, tt::Type{<:Tuple}, world::UInt)`, to construct a
  `MethodInstance` from a tuple type.
- `generate_code_instance(provider::AbstractProvider, mi::MethodInstance)` to emit the `CodeInstance`
  that will hold compilation results to be later retrieved with `LookupResult`.
- `LookupResult(provider::AbstractProvider, src, optimize::Bool)` to assemble most of the information
  that Cthulhu needs to display code and to provide callsites to select for further descent.
- `find_caller_of(provider::AbstractProvider, callee::Union{MethodInstance,Type}, mi::MethodInstance; allow_unspecialized::Bool=false)` to find callers for [`ascend`](@ref).

Optionally, `AbstractProvider`s may extend the defaults by extending the following:
- `should_regenerate_code_instance(provider::AbstractProvider, ci::CodeInstance)`, in case we happened
  to have a `CodeInstance` available, but we still need to process it to get the data we need to provide
  a [`LookupResult`](@ref).


If any of the relevant `CthulhuConfig` fields may be set, these should be implemented as well
or will give a warning by default when called:
- `get_pc_remarks(provider::AbstractProvider, ci::CodeInstance)` (required for the 'remarks' toggle)
- `get_pc_effects(provider::AbstractProvider, ci::CodeInstance)` (required for the 'effects' toggle)
- `get_pc_excts(provider::AbstractProvider, ci::CodeInstance)` (required for the 'exception types' toggle)
- `get_inlining_costs(provider::AbstractProvider, mi::MethodInstance, src::Union{CodeInfo, IRCode})`
  (required for the 'inlining costs' toggle)

`AbstractProvider`s may additionally extend the following UI-related methods:
- `menu_commands(provider::AbstractProvider)` to provide commands for the UI. You will most likely
  want to add or remove commands to `default_menu_commands(provider)` to keep all or part of the
  built-in commands.
- `is_command_enabled(provider::AbstractProvider, state::CthulhuState, command::Command)` to
  dynamically enable/disable commands based on e.g. which view we are in. You will most likely want to fall back to `is_default_command_enabled(provider, state, command)` to keep the behavior for default commands.
- `show_command(io::IO, provider::AbstractProvider, state::CthulhuState, command::Command)` to customize
  the display of commands in the terminal UI. The defaults should be good enough for most uses, however.
  You may take a look at the source code to know how it interacts with other customizable entry points,
  such as `style_for_command_key` and `value_for_command`.

If you do not intend to provide a method that would be required for a specific UI component (for example,
you do not want to support displaying remarks, effects or exception types), you should extend
`menu_commands(provider)` and delete the corresponding command from `default_menu_commands(provider)`.
"""
abstract type AbstractProvider end

get_abstract_interpreter(provider::AbstractProvider) = nothing

function CC.get_inference_world(provider::AbstractProvider)
    interp = get_abstract_interpreter(provider)
    interp !== nothing && return get_inference_world(interp)
    error(lazy"Not implemented for $provider")
end

find_method_instance(provider::AbstractProvider, @nospecialize(f), world::UInt = Base.tls_world_age()) =
    find_method_instance(provider, f, Base.default_tt(f), world)

function find_method_instance(provider::AbstractProvider, @nospecialize(f), @nospecialize(argtypes), world::UInt = Base.tls_world_age())
    # TODO: handle opaque closures
    tt = Base.signature_type(f, argtypes)
    return find_method_instance(provider, tt, world)
end

function find_method_instance(provider::AbstractProvider, @nospecialize(tt::Type{<:Tuple}), world::UInt)
    interp = get_abstract_interpreter(provider)
    interp !== nothing && return find_method_instance(provider, interp, tt, world)
    error(lazy"Not implemented for $provider")
end

function generate_code_instance(provider::AbstractProvider, mi::MethodInstance)
    interp = get_abstract_interpreter(provider)
    interp !== nothing && return generate_code_instance(provider, interp, mi)
    error(lazy"Not implemented for $provider")
end

should_regenerate_code_instance(provider::AbstractProvider, ci::CodeInstance) = false

get_pc_remarks(provider::AbstractProvider, ci::CodeInstance) =
    @warn "Remarks could not be retrieved for $ci for provider $(typeof(provider))"
get_pc_effects(provider::AbstractProvider, ci::CodeInstance) =
    @warn "Effects could not be retrieved for $ci for provider $(typeof(provider))"
get_pc_excts(provider::AbstractProvider, ci::CodeInstance) =
    @warn "Exception types could not be retrieved for $ci for provider $(typeof(provider))"

function find_caller_of(provider::AbstractProvider, callee::Union{MethodInstance,Type}, mi::MethodInstance; allow_unspecialized::Bool=false)
    interp = get_abstract_interpreter(provider)
    interp !== nothing && return find_caller_of(provider, interp, callee, mi, allow_unspecialized)
    error(lazy"Not implemented for $provider")
end

function get_inlining_costs(provider::AbstractProvider, mi::MethodInstance, src::Union{CodeInfo, IRCode})
    interp = get_abstract_interpreter(provider)
    interp !== nothing && return get_inlining_costs(provider, interp, mi, src)
    return nothing
end

function show_parameters(io::IO, provider::AbstractProvider)
    interp = get_abstract_interpreter(provider)
    interp !== nothing && return show_parameters(io, provider, interp)
    error(lazy"Not implemented for $provider")
end

function get_override(provider::AbstractProvider, @nospecialize(info))
    isa(info, ConstPropCallInfo) && return info.result
    isa(info, SemiConcreteCallInfo) && return info
    isa(info, OCCallInfo) && return get_override(provider, info.ci)
    return nothing
end

struct LookupResult
    ir::Union{IRCode, Nothing} # used over `src` for callsite detection and printing
    src::Union{CodeInfo, Nothing} # may be required (e.g. for LLVM and native module dumps)
    rt
    exct
    infos::Vector{Any}
    slottypes::Vector{Any}
    effects::Effects
    optimized::Bool
    function LookupResult(ir, src, @nospecialize(rt), @nospecialize(exct),
                          infos, slottypes, effects, optimized)
        ninfos = length(infos)
        if src === nothing && ir === nothing
            throw(ArgumentError("At least one of `src` or `ir` must have a value"))
        end
        if isa(ir, IRCode) && ninfos ≠ length(ir.stmts)
            throw(ArgumentError("`ir` and `infos` are inconsistent with $(length(ir.stmts)) IR statements and $ninfos call infos"))
        end
        if isa(src, CodeInfo)
            if !isa(src.ssavaluetypes, Vector{Any})
                throw(ArgumentError("`src.ssavaluetypes::$(typeof(src.ssavaluetypes))` must be a Vector{Any}"))
            end
            if !isa(ir, IRCode) && ninfos ≠ length(src.code)
                throw(ArgumentError("`src` and `infos` are inconsistent with $(length(src.code)) code statements and $ninfos call infos"))
            end
        end
        return new(ir, src, rt, exct, infos, slottypes, effects, optimized)
    end
end

function LookupResult(provider::AbstractProvider, src, optimize::Bool)
    interp = get_abstract_interpreter(provider)
    interp !== nothing && return LookupResult(provider, interp, src, optimize)
    error(lazy"""
        missing `$AbstractProvider` API:
        `$(typeof(provider))` is required to implement the `$LookupResult(provider::$(typeof(provider)), src, optimize::Bool)` interface.
    """)
end
