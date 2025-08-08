abstract type AbstractProvider end

get_abstract_interpreter(provider::AbstractProvider) = nothing

function CC.get_inference_world(provider::AbstractProvider)
    interp = get_abstract_interpreter(provider)
    interp !== nothing && return get_inference_world(interp)
    error(lazy"Not implemented for $provider")
end

function find_method_instance(provider::AbstractProvider, @nospecialize(f), world::UInt = Base.tls_world_age())
    find_method_instance(provider, f, Base.default_tt(f), world)
end

function find_method_instance(provider::AbstractProvider, @nospecialize(f), @nospecialize(argtypes), world::UInt = Base.tls_world_age())
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

function get_effects(provider::AbstractProvider, ci::CodeInstance, optimized::Bool)
    error(lazy"Not implemented for $provider")
end

function find_caller_of(provider::AbstractProvider, callee::Union{MethodInstance,Type}, mi::MethodInstance; allow_unspecialized::Bool=true)
    interp = get_abstract_interpreter(provider)
    interp !== nothing && return find_caller_of(provider, interp, callee, caller, allow_unspecialized)
    error(lazy"Not implemented for $provider")
end

function get_inline_costs(provider::AbstractProvider, mi::MethodInstance, src::Union{CodeInfo, IRCode})
    interp = get_abstract_interpreter(provider)
    interp !== nothing && return get_inline_costs(provider, interp, mi, src)
    error(lazy"Not implemented for $provider")
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

# """
#     AbstractCursor

# Required overloads:
# - `Cthulhu.LookupResult(interp::AbstractInterpreter, curs::AbstractCursor, optimize::Bool)`
# - `Cthulhu.lookup_constproped(interp::AbstractInterpreter, curs::AbstractCursor, override::InferenceResult, optimize::Bool)`
# - `Cthulhu.get_ci(curs::AbstractCursor) -> CodeInstance`
# - `Cthulhu.update_cursor(curs::AbstractCursor, mi::MethodInstance)`
# - `Cthulhu.navigate(curs::AbstractCursor, callsite::Callsite) -> AbstractCursor`
# """
# abstract type AbstractCursor end

# get_ci(curs::AbstractCursor) = error(lazy"""
# missing `$AbstractCursor` API:
# `$(typeof(curs))` is required to implement the `$get_ci(curs::$(typeof(curs))) -> CodeInstance` interface.
# """)

struct LookupResult
    src::Union{CodeInfo,IRCode,Nothing}
    rt
    exct
    infos::Vector{CCCallInfo} # needed? -> Callsite?
    slottypes::Vector{Any}
    effects::Effects
    codeinf::Union{Nothing,CodeInfo}
    optimized::Bool
    function LookupResult(src::Union{CodeInfo,IRCode,Nothing}, @nospecialize(rt), @nospecialize(exct),
        infos::Vector{CCCallInfo}, slottypes::Vector{Any},
        effects::Effects, codeinf::Union{Nothing,CodeInfo},
        optimized::Bool)
        return new(src, rt, exct, infos, slottypes, effects, codeinf, optimized)
    end
end

# LookupResult(provider::AbstractProvider, cursor::AbstractCursor, optimize::Bool) =
#     LookupResult(provider, get_ci(cursor), optimize)

function LookupResult(provider::AbstractProvider, src, optimize::Bool)
    interp = get_abstract_interpreter(provider)
    interp !== nothing && return LookupResult(provider, interp, src, optimize)
    error(lazy"""
        missing `$AbstractProvider` API:
        `$(typeof(provider))` is required to implement the `$LookupResult(provider::$(typeof(provider)), src, optimize::Bool)` interface.
    """)
end

# navigate(curs::AbstractCursor, callsite::Callsite) = error(lazy"""
# missing `$AbstractCursor` API:
# `$(typeof(curs))` is required to implement the `$navigate(curs::$(typeof(curs)), callsite::Callsite) -> AbstractCursor` interface.
# """)

# struct CthulhuCursor <: AbstractCursor
#     ci::CodeInstance
# end

# AbstractCursor(provider::AbstractProvider, ci::CodeInstance) = CthulhuCursor(ci)
# get_ci(curs::CthulhuCursor) = curs.ci
# navigate(curs::CthulhuCursor, callsite::Callsite) = CthulhuCursor(get_ci(callsite))

menu_commands(provider::AbstractProvider) = default_menu_commands()
