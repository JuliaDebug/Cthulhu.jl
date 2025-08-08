function find_method_instance(interp::AbstractInterpreter, @nospecialize(tt::Type{<:Tuple}), world::UInt)
    mt = method_table(interp)
    match, valid_worlds = findsup(tt, mt)
    match === nothing && return nothing
    mi = specialize_method(match)
    return mi
end

function generate_code_instance(interp::AbstractInterpreter, mi::MethodInstance)
    ci = run_type_inference(interp, mi)
    return ci
end

function find_caller_of(interp::AbstractInterpreter, callee::Union{MethodInstance,Type}, caller::MethodInstance, allow_unspecialized::Bool)
    interp′ = CthulhuInterpreter(interp)
    codeinst = generate_code_instance(interp′, caller)
    @assert codeinst.def === caller
    locs = Tuple{Core.LineInfoNode,Int}[]
    for optimize in (true, false)
        (; src, rt, infos, slottypes) = LookupResult(interp′, codeinst, optimize)
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

function get_inline_costs(interp::AbstractInterpreter, mi::MethodInstance, src::Union{CodeInfo, IRCode})
    code = isa(src, IRCode) ? src.stmts.stmt : src.code
    costs = zeros(Int, length(code))
    params = CC.OptimizationParams(interp)
    sparams = CC.VarState[CC.VarState(sparam, false) for sparam in mi.sparam_vals]
    CC.statement_costs!(costs, code, src, sparams, params)
end

show_parameters(io::IO, interp::AbstractInterpreter) = show_inference_cache(io, interp)

function show_inference_cache(io::IO, interp::AbstractInterpreter)
    @info "Dumping inference cache."
    cache = CC.get_inference_cache(interp)
    for (i, (; linfo, result)) in enumerate(cache)
        println(io, i, ": ", linfo, "::", result)
    end
end

AbstractProvider(interp::NativeInterpreter) = DefaultProvider(interp)

function AbstractProvider(interp::AbstractInterpreter)
    error(lazy"""missing `$AbstractInterpreter` API:
    `$(typeof(interp))` is required to implement `$AbstractProvider(interp::$(typeof(interp)))) -> AbstractProvider`.
    """)
end
