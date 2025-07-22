abstract type AbstractProvider end

get_abstract_interpreter(provider::AbstractProvider) = nothing

function CC.get_inference_world(provider::AbstractProvider)
    interp = get_abstract_interpreter(provider)
    interp !== nothing && return get_inference_world(interp)
    error(lazy"Not implemented for $provider")
end

function find_method_instance(provider::AbstractProvider, @nospecialize(f), @nospecialize(argtypes), world::UInt = Base.tls_world_age())
    tt = Base.signature_type(f, argtypes)
    return find_method_instance(provider, tt, world)
end

function find_method_instance(provider::AbstractProvider, @nospecialize(tt::Type), world::UInt)
    interp = get_abstract_interpreter(provider)
    interp !== nothing && return find_method_instance(interp, tt, world)
    error(lazy"Not implemented for $provider")
end

function find_method_instance(interp::AbstractInterpreter, @nospecialize(tt::Type), world::UInt)
    mt = method_table(interp)
    match, valid_worlds = findsup(tt, mt)
    match === nothing && return nothing
    mi = specialize_method(match)
    return mi
end

function generate_code_instance(provider::AbstractProvider, mi::MethodInstance)
    interp = get_abstract_interpreter(provider)
    interp !== nothing && return generate_code_instance(interp, mi)
    error(lazy"Not implemented for $provider")
end

function generate_code_instance(interp::AbstractInterpreter, mi::MethodInstance)
    ci = run_type_inference(interp, mi)
    return ci
end

should_regenerate_code_instance(provider::AbstractProvider, ci::CodeInstance) = false

function get_effects(provider::AbstractProvider, ci::CodeInstance, optimized::Bool)
    error(lazy"Not implemented for $provider")
end

function find_caller_of(provider::AbstractProvider, callee::Union{MethodInstance,Type}, mi::MethodInstance; allow_unspecialized::Bool=true)
    interp = get_abstract_interpreter(provider)
    interp !== nothing && return find_caller_of(interp, callee, caller, allow_unspecialized)
    error(lazy"Not implemented for $provider")
end

function find_caller_of(interp::AbstractInterpreter, callee::Union{MethodInstance,Type}, caller::MethodInstance, allow_unspecialized::Bool)
    interp′ = CthulhuInterpreter(interp)
    codeinst = generate_code_instance(interp′, caller)
    @assert codeinst.def === caller
    locs = Tuple{Core.LineInfoNode,Int}[]
    for optimize in (true, false)
        (; src, rt, infos, slottypes) = lookup(interp′, codeinst, optimize)
        callsites, _ = find_callsites(interp′, src, infos, codeinst, slottypes, optimize)
        callsites = allow_unspecialized ? filter(cs->maybe_callsite(cs, callee), callsites) :
        filter(cs->is_callsite(cs, callee), callsites)
        foreach(cs -> add_sourceline!(locs, src, cs.id, caller), callsites)
    end
    # Consolidate by method, but preserve the order
    prlookup = Dict{Tuple{Symbol,Symbol},Int}()
    ulocs = Pair{Tuple{Symbol,Symbol,Int},Vector{Int}}[]
    if !isempty(locs)
        for (loc, depth) in locs
            locname = loc.method
            if isa(locname, MethodInstance)
                locname = locname.def.name
            end
            idx = get(prlookup, (locname, loc.file), nothing)
            if idx === nothing
                push!(ulocs, (locname, loc.file, depth) => Int[])
                prlookup[(locname, loc.file)] = idx = length(ulocs)
            end
            lines = ulocs[idx][2]
            line = loc.line
            if line ∉ lines
                push!(lines, line)
            end
        end
    end
    return ulocs
end

function get_inline_costs(provider::AbstractProvider, mi::MethodInstance, src::Union{CodeInfo, IRCode})
    interp = get_abstract_interpreter(provider)
    interp !== nothing && return get_inline_costs(interp, mi, src)
    error(lazy"Not implemented for $provider")
end

function get_inline_costs(interp::AbstractInterpreter, mi::MethodInstance, src::Union{CodeInfo, IRCode})
    code = isa(src, IRCode) ? src.stmts.stmt : src.code
    costs = zeros(Int, length(code))
    params = CC.OptimizationParams(interp)
    sparams = CC.VarState[CC.VarState(sparam, false) for sparam in mi.sparam_vals]
    CC.statement_costs!(costs, code, src, sparams, params)
end

function show_parameters(io::IO, provider::AbstractProvider)
    interp = get_abstract_interpreter(provider)
    interp !== nothing && return show_parameters(io, interp)
    error(lazy"Not implemented for $provider")
end

show_parameters(io::IO, interp::AbstractInterpreter) = show_inference_cache(io, interp)

function show_inference_cache(io::IO, interp::AbstractInterpreter)
    @info "Dumping inference cache."
    cache = CC.get_inference_cache(interp)
    for (i, (; linfo, result)) in enumerate(cache)
        println(io, i, ": ", linfo, "::", result)
    end
end

"""
    AbstractCursor

Required overloads:
- `Cthulhu.lookup(interp::AbstractInterpreter, curs::AbstractCursor, optimize::Bool)`
- `Cthulhu.lookup_constproped(interp::AbstractInterpreter, curs::AbstractCursor, override::InferenceResult, optimize::Bool)`
- `Cthulhu.get_ci(curs::AbstractCursor) -> CodeInstance`
- `Cthulhu.update_cursor(curs::AbstractCursor, mi::MethodInstance)`
- `Cthulhu.navigate(curs::AbstractCursor, callsite::Callsite) -> AbstractCursor`
"""
abstract type AbstractCursor end

get_ci(curs::AbstractCursor) = error(lazy"""
missing `$AbstractCursor` API:
`$(typeof(curs))` is required to implement the `$get_ci(curs::$(typeof(curs))) -> CodeInstance` interface.
""")

function lookup(provider::AbstractProvider, curs::AbstractCursor, optimize::Bool)
    error(lazy"""
  missing `$AbstractCursor` API:
  `$(typeof(curs))` is required to implement the `$lookup(provider::$(typeof(provider)), curs::$(typeof(curs)), optimize::Bool)` interface.
  """)
end

navigate(curs::AbstractCursor, callsite::Callsite) = error(lazy"""
missing `$AbstractCursor` API:
`$(typeof(curs))` is required to implement the `$navigate(curs::$(typeof(curs)), callsite::Callsite) -> AbstractCursor` interface.
""")

struct CthulhuCursor <: AbstractCursor
    ci::CodeInstance
end

AbstractCursor(provider::AbstractProvider, ci::CodeInstance) = CthulhuCursor(ci)
get_ci(curs::CthulhuCursor) = curs.ci
navigate(curs::CthulhuCursor, callsite::Callsite) = CthulhuCursor(get_ci(callsite))
