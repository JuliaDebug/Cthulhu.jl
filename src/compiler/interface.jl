AbstractProvider(interp::NativeInterpreter) = DefaultProvider(interp)

function AbstractProvider(interp::AbstractInterpreter)
    error(lazy"""missing `$AbstractInterpreter` API:
    `$(typeof(interp))` is required to implement `$AbstractProvider(interp::$(typeof(interp)))) -> AbstractProvider`.
    """)
end

get_inference_world(interp::AbstractInterpreter) = CC.get_inference_world(interp)

function find_method_instance(provider::AbstractProvider, interp::AbstractInterpreter, @nospecialize(tt::Type{<:Tuple}), world::UInt)
    mt = method_table(interp)
    match, valid_worlds = findsup(tt, mt)
    match === nothing && return nothing
    mi = specialize_method(match)
    return mi
end

function generate_code_instance(provider::AbstractProvider, interp::AbstractInterpreter, mi::MethodInstance)
    ci = run_type_inference(provider, interp, mi)
    return ci
end

function run_type_inference end

get_override(provider::AbstractProvider, info::ConstPropCallInfo) = info.result
get_override(provider::AbstractProvider, info::SemiConcreteCallInfo) = info
get_override(provider::AbstractProvider, info::OCCallInfo) = get_override(provider, info.ci)

function get_inlining_costs(provider::AbstractProvider, interp::AbstractInterpreter, mi::MethodInstance, src::Union{CodeInfo, IRCode})
    code = isa(src, IRCode) ? src.stmts.stmt : src.code
    costs = zeros(Int, length(code))
    params = CC.OptimizationParams(interp)
    sparams = CC.VarState[CC.VarState(sparam, false) for sparam in mi.sparam_vals]
    CC.statement_costs!(costs, code, src, sparams, params)
    return costs
end

show_parameters(io::IO, provider::AbstractProvider, interp::AbstractInterpreter) = show_inference_cache(io, interp)

function show_inference_cache(io::IO, interp::AbstractInterpreter)
    @info "Dumping inference cache."
    cache = CC.get_inference_cache(interp)
    for (i, (; linfo, result)) in enumerate(cache)
        println(io, i, ": ", linfo, "::", result)
    end
end

function find_caller_of(provider::AbstractProvider, interp::AbstractInterpreter, callee::Union{MethodInstance,Type}, mi::MethodInstance, allow_unspecialized::Bool)
    ci = generate_code_instance(provider, interp, mi)
    @assert get_mi(ci) === mi
    locs = Tuple{Core.LineInfoNode,Int}[]
    for optimize in (true, false)
        result = lookup(provider, interp, ci, optimize)
        callsites, _ = find_callsites(provider, result, ci)
        callsites = allow_unspecialized ? filter(cs -> maybe_callsite(cs, callee), callsites) :
        filter(cs -> is_callsite(cs, callee), callsites)
        foreach(cs -> add_sourceline!(locs, result.src, cs.id, mi), callsites)
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
