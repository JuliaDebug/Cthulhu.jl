AbstractProvider(interp::NativeInterpreter) = DefaultProvider(interp)

function AbstractProvider(interp::AbstractInterpreter)
    error(lazy"""missing `$AbstractInterpreter` API:
    `$(typeof(interp))` is required to implement `$AbstractProvider(interp::$(typeof(interp)))) -> AbstractProvider`.
    """)
end

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

function find_caller_of(provider::AbstractProvider, interp::AbstractInterpreter, callee::Union{MethodInstance,Type}, mi::MethodInstance, allow_unspecialized::Bool)
    ci = generate_code_instance(provider, interp, mi)
    @assert get_mi(ci) === mi
    locs = Tuple{Core.LineInfoNode,Int}[]
    for optimize in (true, false)
        result = LookupResult(provider, interp, ci, optimize)
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

function get_inline_costs(provider::AbstractProvider, interp::AbstractInterpreter, mi::MethodInstance, src::Union{CodeInfo, IRCode})
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

function LookupResult(provider::AbstractProvider, interp::AbstractInterpreter, ci::CodeInstance, optimize::Bool)
    if optimize
        result = lookup_optimized(provider, interp, ci)
        if result === nothing
            @info """
            Inference discarded the source for this call because of recursion:
            Cthulhu nevertheless is trying to retrieve the source for further inspection.
            """
            result = lookup_unoptimized(provider, interp, ci)
        end
        return result
    end
    return lookup_unoptimized(provider, interp, ci)
end

function LookupResult(provider::AbstractProvider, interp::AbstractInterpreter, result::InferenceResult, optimize::Bool)
    optimize && return lookup_constproped_optimized(provider, interp, result)
    return lookup_constproped_unoptimized(provider, interp, result)
end

function LookupResult(provider::AbstractProvider, call::SemiConcreteCallInfo, optimize::Bool)
    return lookup_semiconcrete(provider, interp, call)
end

struct InferredSource
    src::CodeInfo
    stmt_info::Vector{CCCallInfo}
    effects::Effects
    rt::Any
    exct::Any
    function InferredSource(src::CodeInfo, stmt_info::Vector{CCCallInfo}, effects, @nospecialize(rt), @nospecialize(exct))
        new(src, stmt_info, effects, rt, exct)
    end
end

struct OptimizedSource
    ir::IRCode
    src::CodeInfo
    isinlineable::Bool
    effects::Effects
end

const InferenceKey = Union{CodeInstance,InferenceResult} # TODO make this `CodeInstance` fully
const InferenceDict{InferenceValue} = IdDict{InferenceKey, InferenceValue}
const PC2Remarks = Vector{Pair{Int, String}}
const PC2CallMeta = Dict{Int, CallMeta}
const PC2Effects = Dict{Int, Effects}
const PC2Excts = Dict{Int, Any}

get_pc_remarks(provider::AbstractProvider, ci::CodeInstance) = nothing
get_pc_effects(provider::AbstractProvider, ci::CodeInstance) = nothing
get_pc_excts(provider::AbstractProvider, ci::CodeInstance) = nothing

function lookup_optimized(provider::AbstractProvider, interp::AbstractInterpreter, ci::CodeInstance)
    rt = cached_return_type(ci)
    exct = cached_exception_type(ci)
    effects = get_effects(ci)
    if CC.use_const_api(ci) && ci.inferred === nothing
        @assert isdefined(ci, :rettype_const)
        src = CC.codeinfo_for_const(interp, get_mi(ci), ci.rettype_const)
        infos = CCCallInfo[]
        slottypes = Any[]
        return LookupResult(src, rt, exct, infos, slottypes, effects, src, true)
    end
    opt = OptimizedSource(provider, interp, ci)
    ir = CC.copy(opt.ir)
    codeinf = opt.src
    infos = ir.stmts.info
    slottypes = ir.argtypes
    return LookupResult(ir, rt, exct, infos, slottypes, effects, codeinf, true)
end

function lookup_unoptimized(provider::AbstractProvider, interp::AbstractInterpreter, ci::CodeInstance)
    unopt = InferredSource(provider, interp, ci)
    codeinf = src = copy(unopt.src)
    (; rt, exct) = unopt
    infos = unopt.stmt_info
    effects = unopt.effects
    slottypes = src.slottypes
    if isnothing(slottypes)
        slottypes = Any[ Any for i = 1:length(src.slotflags) ]
    end
    return LookupResult(src, rt, exct, infos, slottypes, effects, codeinf, false)
end

function lookup_constproped_optimized(provider::AbstractProvider, interp::AbstractInterpreter, override::InferenceResult)
    opt = OptimizedSource(provider, interp, override)
    # `override.src` might has been transformed to OptimizedSource already,
    # e.g. when we switch from constant-prop' unoptimized source
    src = CC.copy(opt.ir)
    rt = override.result
    exct = override.exc_result
    infos = src.stmts.info
    slottypes = src.argtypes
    codeinf = opt.src
    effects = opt.effects
    return LookupResult(src, rt, exct, infos, slottypes, effects, codeinf, true)
end

function lookup_constproped_unoptimized(provider::AbstractProvider, interp::AbstractInterpreter, override::InferenceResult)
    unopt = InferredSource(provider, interp, override)
    codeinf = src = copy(unopt.src)
    (; rt, exct) = unopt
    infos = unopt.stmt_info
    effects = get_effects(unopt)
    slottypes = src.slottypes
    if isnothing(slottypes)
        slottypes = Any[ Any for i = 1:length(src.slotflags) ]
    end
    return LookupResult(src, rt, exct, infos, slottypes, effects, codeinf, false)
end

function lookup_semiconcrete(provider::AbstractProvider, interp::AbstractInterpreter, override::SemiConcreteCallInfo)
    src = CC.copy(override.ir)
    rt = get_rt(override)
    exct = Any # TODO
    infos = src.stmts.info
    slottypes = src.argtypes
    effects = get_effects(override)
    codeinf = nothing # TODO try to find `CodeInfo` for the regular inference?
    return LookupResult(src, rt, exct, infos, slottypes, effects, codeinf, true)
end

function run_type_inference(provider::AbstractProvider, interp::AbstractInterpreter, mi::MethodInstance)
    result = InferenceResult(mi)
    ci = CC.engine_reserve(interp, mi)
    result.ci = ci
    # we may want to handle the case when `InferenceState(...)` returns `nothing`,
    # which indicates code generation of a `@generated` has been failed,
    # and show it in the UI in some way?
    frame = InferenceState(result, #=cache_mode=#:global, interp)::InferenceState
    CC.typeinf(interp, frame)
    return ci
end
