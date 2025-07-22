struct DefaultProvider <: AbstractProvider
    interp::CthulhuInterpreter
end
DefaultProvider(interp::CC.AbstractInterpreter = NativeInterpreter()) = DefaultProvider(CthulhuInterpreter(interp))

get_abstract_interpreter(provider::DefaultProvider) = provider.interp

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

# Interface.

function lookup(provider::DefaultProvider, ci::CodeInstance, optimize::Bool)
    if optimize
        result = lookup_optimized(provider.interp, ci)
        if result === nothing
            @info """
            Inference discarded the source for this call because of recursion:
            Cthulhu nevertheless is trying to retrieve the source for further inspection.
            """
            result = lookup_unoptimized(provider.interp, ci)
        end
        return result
    end
    return lookup_unoptimized(provider.interp, ci)
end

function lookup(provider::DefaultProvider, result::InferenceResult, optimize::Bool)
    optimize && return lookup_constproped_optimized(provider.interp, result)
    return lookup_constproped_unoptimized(provider.interp, result)
end

function lookup(provider::DefaultProvider, call::SemiConcreteCallInfo, optimize::Bool)
    return lookup_semiconcrete(provider.interp, call)
end

function get_override(provider::DefaultProvider, @nospecialize(info))
    isa(info, ConstPropCallInfo) && return info.result
    isa(info, SemiConcreteCallInfo) && return info
    isa(info, OCCallInfo) && return get_override(info.ci)
    return nothing
end

function should_regenerate_code_instance(provider::DefaultProvider, ci::CodeInstance)
    return !haskey(provider.interp.unopt, ci)
end

get_pc_remarks(provider::AbstractProvider, key::InferenceKey) = nothing
get_pc_remarks(provider::DefaultProvider, key::InferenceKey) = get(provider.interp.remarks, key, nothing)

get_pc_effects(provider::AbstractProvider, key::InferenceKey) = nothing
get_pc_effects(provider::DefaultProvider, key::InferenceKey) = get(provider.interp.effects, key, nothing)

get_pc_exct(provider::AbstractProvider, key::InferenceKey) = nothing
get_pc_exct(provider::DefaultProvider, key::InferenceKey) = get(provider.interp.exception_types, key, nothing)

# Implementation.

function lookup_optimized(interp::CthulhuInterpreter, ci::CodeInstance)
    rt = cached_return_type(ci)
    exct = cached_exception_type(ci)
    opt = ci.inferred
    if opt !== nothing
        opt = opt::OptimizedSource
        src = CC.copy(opt.ir)
        codeinf = opt.src
        infos = src.stmts.info
        slottypes = src.argtypes
    elseif CC.use_const_api(ci)
        @assert isdefined(ci, :rettype_const)
        const_ci = CC.codeinfo_for_const(interp, get_mi(ci), ci.rettype_const)
        return lookup_optimized(interp, const_ci)
    else
        Core.eval(Main, quote
            interp = $interp
            ci = $ci
        end)
        error("couldn't find the source; inspect `Main.interp` and `Main.mi`")
    end
    effects = get_effects(ci)
    return LookupResult(src, rt, exct, infos, slottypes, effects, codeinf, true)
end

function lookup_unoptimized(interp::CthulhuInterpreter, ci::CodeInstance)
    unopt = interp.unopt[ci]
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

function lookup_constproped_optimized(interp::CthulhuInterpreter, override::InferenceResult)
    opt = override.src
    isa(opt, OptimizedSource) || error("couldn't find the source")
    # `(override::InferenceResult).src` might has been transformed to OptimizedSource already,
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

function lookup_constproped_unoptimized(interp::CthulhuInterpreter, override::InferenceResult)
    unopt = interp.unopt[override]
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

function lookup_semiconcrete(interp::CthulhuInterpreter, override::SemiConcreteCallInfo)
    src = CC.copy(override.ir)
    rt = get_rt(override)
    exct = Any # TODO
    infos = src.stmts.info
    slottypes = src.argtypes
    effects = get_effects(override)
    codeinf = nothing # TODO try to find `CodeInfo` for the regular inference?
    return LookupResult(src, rt, exct, infos, slottypes, effects, codeinf, true)
end

function run_type_inference(interp::AbstractInterpreter, mi::MethodInstance)
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
