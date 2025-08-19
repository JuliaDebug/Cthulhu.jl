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

get_pc_remarks(provider::AbstractProvider, ci::CodeInstance) = nothing
get_pc_effects(provider::AbstractProvider, ci::CodeInstance) = nothing
get_pc_excts(provider::AbstractProvider, ci::CodeInstance) = nothing

function find_caller_of(provider::AbstractProvider, callee::Union{MethodInstance,Type}, mi::MethodInstance; allow_unspecialized::Bool=false)
    interp = get_abstract_interpreter(provider)
    interp !== nothing && return find_caller_of(provider, interp, callee, mi, allow_unspecialized)
    error(lazy"Not implemented for $provider")
end

function get_inlining_costs(provider::AbstractProvider, mi::MethodInstance, src::Union{CodeInfo, IRCode})
    interp = get_abstract_interpreter(provider)
    interp !== nothing && return get_inlining_costs(provider, interp, mi, src)
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
