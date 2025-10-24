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
- `CodeInfo`, representing Julia code, as output e.g. by `@code_typed`.

And a few from the compiler:
- `Compiler.IRCode`, somewhat similar to `CodeInfo`, but in a different representation.
- `Compiler.Effects`, code metadata used to model part of its semantics and behavior.
- `Compiler.InferenceResult`, holding the results of inference.

Cthulhu is integrated with the Julia compiler, and allows reuse
of its mechanisms to generate these data structures. In particular,
default methods are defined for `AbstractProvider`s that are associated with
a `Compiler.AbstractInterpreter` (the interface type for Compiler, analogous
to `AbstractProvider` for Cthulhu). This allows to reuse some of the commonly
used patterns that are expected with the majority of Compiler integrations.
The association is made by returning an `AbstractInterpreter` with
`Cthulhu.get_abstract_interpreter(provider::SomeProvider)`, which by default
returns `nothing` (no association).

The reverse is implemented as well: sometimes, an `AbstractInterpreter` implemented
by a package is central enough that it is part of its API. In this case, this package
may define a constructor `Cthulhu.AbstractProvider(interp::SomeInterpreter)` to automatically
pick the correct provider just by doing `@descend interp=SomeInterpreter() f(x)`.

The interface for `AbstractProvider` requires a few methods to be defined. If it is associated
with an `AbstractInterpreter` (see two paragraphs above), then only the following is required:
- `run_type_inference(provider::SomeProvider, interp::SomeInterpreter, mi::MethodInstance)` to emit a `CodeInstance`
  by invoking regular type inference, typically calling `typeinf_` methods from Compiler.
- `OptimizedSource(provider::SomeProvider, interp::SomeInterpreter, ci::CodeInstance)`
- `OptimizedSource(provider::SomeProvider, interp::SomeInterpreter, result::InferenceResult)`
  (for uncached inference results, e.g. when looking up callsites emanating from concrete evaluation)
- `InferredSource(provider::SomeProvider, interp::SomeInterpreter, ci::CodeInstance)`

By default, an `AbstractProvider` is not associated with any particular `Compiler.AbstractInterpreter`,
with `Cthulhu.get_abstract_interpreter(provider::SomeProvider)` returning `nothing`. In this case,
the following methods are required:
- `get_inference_world(provider::SomeProvider)`, returning the world in which the provider operates. Methods and bindings (including e.g. types and globals) defined after this world age should not be taken into account when providing the data Cthulhu needs.
- `find_method_instance(provider::SomeProvider, tt::Type{<:Tuple}, world::UInt)`, to return the specialization of the matching method for `tt`, returning a `MethodInstance`.
- `generate_code_instance(provider::SomeProvider, mi::MethodInstance)` to emit the `CodeInstance`
  that holds compilation results to be later retrieved with `LookupResult`.
- `LookupResult(provider::SomeProvider, src, optimize::Bool)` to assemble most of the information
  that Cthulhu needs to display code and to provide callsites to select for further descent. `src`
  is the generated `CodeInstance` or any override emanating from `get_override(provider::SomeProvider, info::Cthulhu.CallInfo)` (more on that a few paragraphs below).
  which by default may return `InferenceResult` (constant propagation) or `SemiConcreteCallInfo` (semi-concrete evaluation). This "override" replaces a `CodeInstance` for callsites
  that are semantically associated with one but which have further information available, or for which the compiler decides to not cache one. 
- `find_caller_of(provider::SomeProvider, callee::Union{MethodInstance,Type}, mi::MethodInstance; allow_unspecialized::Bool=false)` to find occurrences of calls to `callee` in the provided method instance. This is the backbone of [`ascend`](@ref), and is not used for `descend`.

Callsites are retrieved by inspecting `Compiler.CallInfo` data structures (which are currently distinct from `Cthulhu.CallInfo`, see the warning below), which are metadata
semantically attached to expressions, relating one to the process that produced them.
In most cases, expressions semantically related to a call to another function will have a `Compiler.CallInfo` attached
that maps to the relevant `CodeInstance`. However, it may be useful to note that there may be cases where a `CodeInstance` is not readily available: that is the case for
constant propagation (the original `CodeInstance` is not kept if the result is
inferred to a constant), and for semi-concrete evaluation which produces temporary refined IR
that contains more accurate information than the `CodeInstance` it originates from.

When selecting a callsite, a `LookupResult` is constructed from the data that originates from the corresponding `Cthulhu.CallInfo` structure (which is derived from the `Compiler.CallInfo` metadata).
Generally, this will be a `CodeInstance`, but it may be overriden with another source, depending on the implementation of
`get_override(provider::SomeProvider, info::Cthulhu.CallInfo)` (which may be extended for custom providers).
By default, the override will be either a `Compiler.SemiConcreteCallInfo` for semi-concrete
evaluation, or a `Compiler.InferenceResult` for the results of constant propagation.

!!! warn
    `Compiler.CallInfo` and `Cthulhu.CallInfo` are currently distinct abstract types.
    This part of Cthulhu will likely be subject to change, and is not yet publicly
    available for customization by `AbstractProvider`s beyond its downstream use via `get_override`.
    If feeling adventurous, you may extend the `Cthulhu.find_callsites` internal function,
    however you are encouraged to file an issue and/or contribute to making this part more
    accessible by breaking it into well-interfaced parts instead of duplicating its implementation.

Optionally, `AbstractProvider`s may modify default behavior by extending the following:
- `get_override(provider::SomeProvider, info::Cthulhu.CallInfo)`: return a data structure that is
  more accurate than the `CodeInstance` associated with a given `Cthulhu.CallInfo`.

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
`Cthulhu.menu_commands(provider::SomeProvider)` and return a list of commands that excludes the
corresponding command. To that effect, you can use `Cthulhu.default_menu_commands(provider)`
and simply filter out the relevant command.
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
    @warn "Remarks could not be retrieved for $ci with provider type $(typeof(provider))"
get_pc_effects(provider::AbstractProvider, ci::CodeInstance) =
    @warn "Effects could not be retrieved for $ci with provider type $(typeof(provider))"
get_pc_excts(provider::AbstractProvider, ci::CodeInstance) =
    @warn "Exception types could not be retrieved for $ci with provider type $(typeof(provider))"

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
    @warn "Not implemented for provider type $(typeof(provider))"
end
