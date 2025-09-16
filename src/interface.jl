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
- `CodeInfo`, representing Julia code as output by `@code_typed` and similar macros.

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
`Cthulhu.get_abstract_interpreter(provider::SomeProvider)`.

The reverse is implemented as well: sometimes, an `AbstractInterpreter` implemented
by a package is central enough that it is part of its API. In this case, this package
may define a constructor `Cthulhu.AbstractProvider(interp::SomeInterpreter)` to automatically
pick the correct provider just by doing `@descend interp=SomeInterpreter() f(x)`.

The interface for `AbstractProvider` requires a few methods to be defined. If it supports
`Cthulhu.get_abstract_interpreter(provider::SomeProvider)::SomeInterpreter`,
then only the following is required:
- `OptimizedSource(provider::SomeProvider, interp::SomeInterpreter, ci::CodeInstance)`
- `OptimizedSource(provider::SomeProvider, interp::SomeInterpreter, result::InferenceResult)`
  (for uncached inference results, e.g. when looking up callsites emanating from concrete evaluation)
- `InferredSource(provider::SomeProvider, interp::SomeInterpreter, ci::CodeInstance)`

With an optional method to specify how to run type inference:
- `run_type_inference(provider::SomeProvider, interp::SomeInterpreter, mi::MethodInstance)` to emit a `CodeInstance`
  by invoking regular type inference, typically calling `typeinf_` methods from Compiler. By default, this uses
  `Compiler.typeinf_ext`.

For an `AbstractProvider` that is not associated with any particular `Compiler.AbstractInterpreter`,
the following methods are required:
- `get_inference_world(provider::SomeProvider)`, returning the world in which the provider operates.
- `find_method_instance(provider::SomeProvider, tt::Type{<:Tuple}, world::UInt)`, to construct a
  `MethodInstance` from a tuple type.
- `generate_code_instance(provider::SomeProvider, mi::MethodInstance)` to emit the `CodeInstance`
  that will hold compilation results to be later retrieved with `LookupResult`.
- `LookupResult(provider::SomeProvider, src, optimize::Bool)` to assemble most of the information
  that Cthulhu needs to display code and to provide callsites to select for further descent. `src`
  is the generated `CodeInstance` or any override emanating from `get_override(provider::SomeProvider, info::Compiler.CallInfo)`.
  which by default may return `InferenceResult` (constant propagation) or `SemiConcreteCallInfo` (semi-concrete evaluation). This "override" replaces a `CodeInstance` for callsites
  that are semantically associated with one but which lost 
- `find_caller_of(provider::SomeProvider, callee::Union{MethodInstance,Type}, mi::MethodInstance; allow_unspecialized::Bool=false)` to find callers for [`ascend`](@ref).

Callsites are retrieved by inspecting `Compiler.CallInfo` data structures, which are expression-level annotations relating an expression
to the process that produced them. In most cases, expressions semantically related to a call to another function will have a `CallInfo`
that maps to the relevant `CodeInstance`. However, there may be cases where a `CodeInstance` is not readily available: that is the case for
constant propagation (the original `CodeInstance` is not kept if the result is inferred to a constant), and for semi-concrete evaluation
which produces refined IR to look into instead.

When selecting a callsite, a `LookupResult` is constructed from the data that originates from the corresponding `CallInfo` structure.
Generally, this will be a `CodeInstance`, but it may be overriden with another source, depending on the implementation of
`get_override(provider::SomeProvider, info::Compiler.CallInfo)` (which may be extended for custom providers).
By default, the override will be either a `Compiler.SemiConcreteCallInfo` for semi-concrete evaluation, or a `Compiler.InferenceResult`
for the results of constant propagation.

Optionally, `AbstractProvider`s may extend the defaults by extending the following:
- `should_regenerate_code_instance(provider::SomeProvider, ci::CodeInstance)`, in case we happened
  to have a `CodeInstance` available, but we still need to process it to get the data we need to provide
  a [`LookupResult`](@ref).

If any of the relevant `CthulhuConfig` fields may be set, these should be implemented as well
or will give a warning by default when called:
- `get_pc_remarks(provider::SomeProvider, ci::CodeInstance)` (required for the 'remarks' toggle)
- `get_pc_effects(provider::SomeProvider, ci::CodeInstance)` (required for the 'effects' toggle)
- `get_pc_excts(provider::SomeProvider, ci::CodeInstance)` (required for the 'exception types' toggle)
- `get_inlining_costs(provider::SomeProvider, mi::MethodInstance, src::Union{CodeInfo, IRCode})`
  (required for the 'inlining costs' toggle)

`AbstractProvider`s may additionally extend the following UI-related methods:
- `menu_commands(provider::SomeProvider)` to provide commands for the UI. You will most likely
  want to add or remove commands to `default_menu_commands(provider)` to keep all or part of the
  built-in commands.
- `is_command_enabled(provider::SomeProvider, state::CthulhuState, command::Command)` to
  dynamically enable/disable commands based on e.g. which view we are in. You will most likely want to fall back to
  `Cthulhu.is_default_command_enabled(provider, state, command)` to keep the behavior for default commands.
- `show_command(io::IO, provider::SomeProvider, state::CthulhuState, command::Command)` to customize
  the display of commands in the terminal UI. The defaults should be good enough for most uses, however.
  You may take a look at the source code to know how it interacts with other customizable entry points,
  such as `style_for_command_key` and `value_for_command`.

If you do not intend to provide a method that would be required for a specific UI component (for example,
you do not want to support displaying remarks, effects or exception types), you should extend
`Cthulhu.menu_commands(provider::SomeProvider)` and delete the corresponding command from `Cthulhu.default_menu_commands(provider)`.
"""
abstract type AbstractProvider end

get_abstract_interpreter(provider::AbstractProvider) = nothing

function get_inference_world(provider::AbstractProvider)
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

get_override(provider::AbstractProvider, @nospecialize(info)) = nothing

function lookup(provider::AbstractProvider, src, optimize::Bool)
    interp = get_abstract_interpreter(provider)
    interp !== nothing && return lookup(provider, interp, src, optimize)
    error(lazy"""
        missing `$AbstractProvider` API:
        `$(typeof(provider))` is required to implement the `$LookupResult(provider::$(typeof(provider)), src, optimize::Bool)` interface.
    """)
end

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

function get_inlining_costs(provider::AbstractProvider, mi::MethodInstance, src#=::Union{CodeInfo, CC.IRCode}=#)
    interp = get_abstract_interpreter(provider)
    interp !== nothing && return get_inlining_costs(provider, interp, mi, src)
    return nothing
end

function show_parameters(io::IO, provider::AbstractProvider)
    interp = get_abstract_interpreter(provider)
    interp !== nothing && return show_parameters(io, provider, interp)
    error(lazy"Not implemented for $provider")
end
