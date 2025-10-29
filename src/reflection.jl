###
# Reflection tooling
##

using Base.Meta
using Base: may_invoke_generator

transform(::Val, callsite) = callsite
function transform(::Val{:CuFunction}, provider, callsite, callexpr, src, mi, slottypes; world=get_world_counter())
    sptypes = sptypes_from_meth_instance(mi)
    tt = argextype(callexpr.args[4], src, sptypes, slottypes)
    ft = argextype(callexpr.args[3], src, sptypes, slottypes)
    isa(tt, Const) || return callsite
    sig = Tuple{widenconst(ft), tt.val.parameters...}
    return Callsite(callsite.id, CuCallInfo(callinfo(provider, sig, Nothing; world)), callsite.head)
end

function find_callsites(provider::AbstractProvider, result::LookupResult, ci::CodeInstance, annotate_source::Bool=false, pc2excts::Union{Nothing,PC2Excts}=nothing)
    mi = get_mi(ci)
    sptypes = sptypes_from_meth_instance(mi)
    callsites, sourcenodes = Callsite[], Union{TypedSyntax.MaybeTypedSyntaxNode,Callsite}[]
    src = something(result.ir, result.src)::Union{IRCode, CodeInfo}
    stmts = isa(src, IRCode) ? src.stmts.stmt : src.code
    nstmts = length(stmts)
    _, mappings = annotate_source ? get_typed_sourcetext(mi, src, nothing; warn=false) : (nothing, nothing)

    for id = 1:nstmts
        stmt = stmts[id]
        isa(stmt, Expr) || continue
        callsite = nothing
        if is_call_expr(stmt, result.optimized)
            info = result.infos[id]
            if info !== nothing
                if isa(info, CC.UnionSplitApplyCallInfo)
                    info = something(unpack_cthulhuinfo_from_unionsplit(info), info)
                end
                if isa(info, CthulhuCallInfo)
                    # We have a `CallMeta` available.
                    (; info, rt, exct, effects) = info.meta
                    @assert !isa(info, CthulhuCallInfo)
                else
                    rt = ignorelimited(argextype(SSAValue(id), src, sptypes, result.slottypes))
                    exct = isnothing(pc2excts) ? nothing : get(pc2excts, id, nothing)
                    effects = nothing
                end
                # in unoptimized IR, there may be `slot = rhs` expressions, which `argextype` doesn't handle
                # so extract rhs for such an case
                local args = stmt.args
                if !result.optimized
                    args = (ignorelhs(stmt)::Expr).args
                end
                argtypes = Vector{Any}(undef, length(args))
                ft = ignorelimited(argextype(args[1], src, sptypes, result.slottypes))
                f = CC.singleton_type(ft)
                f === Core.Intrinsics.llvmcall && continue
                f === Core.Intrinsics.cglobal && continue
                argtypes[1] = ft
                for i = 2:length(args)
                    t = argextype(args[i], src, sptypes, result.slottypes)
                    argtypes[i] = ignorelimited(t)
                end
                callinfos = process_info(provider, result, info, argtypes, rt, exct, effects)
                isempty(callinfos) && continue
                callsite = let
                    if length(callinfos) == 1
                        callinfo = callinfos[1]
                    else
                        callinfo = MultiCallInfo(argtypes_to_type(argtypes), rt, callinfos)
                    end
                    Callsite(id, callinfo, stmt.head)
                end
            end
        end

        if callsite === nothing
            local stmt = ignorelhs(stmt)
            (stmt isa Expr) || continue
            (; head, args) = stmt
            if head === :invoke
                rt = argextype(SSAValue(id), src, sptypes, result.slottypes)
                arg1 = args[1]::CodeInstance
                effects = get_effects(arg1) # TODO
                callsite = Callsite(id, EdgeCallInfo(arg1, rt, effects), head)
            elseif head === :foreigncall
                # special handling of jl_new_task
                length(args) > 0 || continue
                arg1 = args[1]
                isexpr(arg1, :tuple) && (arg1 = arg1.args[1])
                if arg1 isa QuoteNode
                    cfunc = arg1.value
                    if cfunc === :jl_new_task
                        func = args[6]
                        ftype = widenconst(argextype(func, src, sptypes, result.slottypes))
                        sig = Tuple{ftype}
                        callsite = Callsite(id, TaskCallInfo(callinfo(provider, sig, nothing; world=get_inference_world(provider))), head)
                    end
                end
            end
        end

        if callsite !== nothing
            info = callsite.info
            if info isa EdgeCallInfo
                ci = get_ci(info)
                meth = ci.def.def
                if isa(meth, Method) && nameof(meth.module) === :CUDAnative && meth.name === :cufunction
                    callsite = transform(Val(:CuFunction), provider, callsite, c, src, ci.def, result.slottypes; world=get_inference_world(provider))
                end
            end

            push!(callsites, callsite)
            if annotate_source
                if mappings !== nothing && checkbounds(Bool, mappings, id)
                    mapped = mappings[id]
                    push!(sourcenodes, length(mapped) == 1 ? tag_runtime(mapped[1], callsite.info) : callsite)
                else
                    push!(sourcenodes, callsite)
                end
            end
        end
    end
    return callsites, sourcenodes
end

function process_const_info(provider::AbstractProvider, ::LookupResult, @nospecialize(thisinfo),
    argtypes::ArgTypes, @nospecialize(rt), @nospecialize(result),
    @nospecialize(exct))
    if isnothing(result)
        return thisinfo
    elseif result isa CC.VolatileInferenceResult
        # NOTE we would not hit this case since `finish!(::CthulhuInterpreter, frame::InferenceState)`
        #      will always transform `frame.result.src` to `OptimizedSource` when frame is inferred
        return thisinfo
    elseif isa(result, CC.ConcreteResult)
        edge = result.edge
        effects = get_effects(result)
        mici = EdgeCallInfo(edge, rt, effects, exct)
        return ConcreteCallInfo(mici, argtypes)
    elseif isa(result, CC.ConstPropResult)
        effects = get_effects(result)
        result = result.result
        mici = EdgeCallInfo(result.ci_as_edge, rt, effects, exct)
        return ConstPropCallInfo(mici, result)
    elseif isa(result, CC.SemiConcreteResult)
        effects = get_effects(result)
        mici = EdgeCallInfo(result.edge, rt, effects, exct)
        return SemiConcreteCallInfo(mici, result.ir)
    else
        @assert isa(result, CC.InferenceResult)
        effects = get_effects(result)
        mici = EdgeCallInfo(result.ci_as_edge, rt, effects, exct)
        return ConstPropCallInfo(mici, result)
    end
end

function process_info(provider::AbstractProvider, result::LookupResult, @nospecialize(info::CCCallInfo),
                      argtypes::ArgTypes, @nospecialize(rt),
                      @nospecialize(exct), effects::Union{Effects, Nothing})
    process_recursive(@nospecialize(newinfo)) = process_info(provider, result, newinfo, argtypes, rt, exct, effects)

    if isa(info, CC.ModifyOpInfo)
        # Just ignore it.
        info = info.info
    end

    if isa(info, MethodResultPure)
        if isa(info.info, CC.ReturnTypeCallInfo)
            # xref: https://github.com/JuliaLang/julia/pull/45299#discussion_r871939049
            info = info.info # cascade to the special handling below
        else
            return CallInfo[PureCallInfo(argtypes, rt)]
        end
    end
    if isa(info, MethodMatchInfo)
        return CallInfo[let
            if edge === nothing
                RTCallInfo(unwrapconst(argtypes[1]), argtypes[2:end], rt, exct)
            else
                effects = @something(effects, get_effects(edge))
                EdgeCallInfo(edge, rt, effects, exct)
            end
        end for edge in info.edges if edge !== nothing]
    elseif isa(info, UnionSplitInfo)
        return mapreduce(process_recursive, vcat, info.split; init=CallInfo[])::Vector{CallInfo}
    elseif isa(info, UnionSplitApplyCallInfo)
        return mapreduce(process_recursive, vcat, info.infos; init=CallInfo[])::Vector{CallInfo}
    elseif isa(info, ApplyCallInfo)
        # XXX: This could probably use its own info.
        r = vcat(process_recursive(info.call), reduce(vcat,
            Any[mapreduce(meta->process_recursive(meta.info), vcat, arg.each)
                for arg in info.arginfo if arg !== nothing], init=Any[]))
        return r
    elseif isa(info, ConstCallInfo)
        infos = process_recursive(info.call)
        @assert length(infos) == length(info.results)
        return CallInfo[let
            process_const_info(provider, result, infos[i], argtypes, rt, value, exct)
        end for (i, value) in enumerate(info.results)]
    elseif isa(info, CC.InvokeCallInfo)
        edge = info.edge
        if edge !== nothing
            effects = @something(effects, get_effects(edge))
            thisinfo = EdgeCallInfo(edge, rt, effects)
            innerinfo = process_const_info(provider, result, thisinfo, argtypes, rt, info.result, exct)
        else
            innerinfo = RTCallInfo(unwrapconst(argtypes[1]), argtypes[2:end], rt, exct)
        end
        info = InvokeCallInfo(innerinfo)
        return CallInfo[info]
    elseif isa(info, CC.OpaqueClosureCallInfo)
        edge = info.edge
        if edge !== nothing
            effects = @something(effects, get_effects(edge))
            thisinfo = EdgeCallInfo(edge, rt, effects)
            innerinfo = process_const_info(provider, result, thisinfo, argtypes, rt, info.result, exct)
        else
            innerinfo = RTCallInfo(unwrapconst(argtypes[1]), argtypes[2:end], rt, exct)
        end
        info = OCCallInfo(innerinfo)
        return CallInfo[info]
    elseif isa(info, CC.GlobalAccessInfo)
        return CallInfo[] # TODO return something informative here?
    elseif isa(info, CC.OpaqueClosureCreateInfo)
        # TODO: Add ability to descend into OCs at creation site
        return CallInfo[]
    elseif isa(info, CC.FinalizerInfo)
        # TODO: Add ability to descend into finalizers at creation site
        return CallInfo[]
    elseif isa(info, CC.ReturnTypeCallInfo)
        newargtypes = argtypes[2:end]
        callinfos = process_info(provider, result, info.info, newargtypes, unwrapType(widenconst(rt)), exct, effects)
        if length(callinfos) == 1
            vmi = only(callinfos)
        else
            @assert isempty(callinfos)
            argt = unwrapType(widenconst(newargtypes[2]))::DataType
            sig = Tuple{widenconst(newargtypes[1]), argt.parameters...}
            vmi = FailedCallInfo(sig, Union{})
        end
        return CallInfo[ReturnTypeCallInfo(vmi)]
    elseif info == NoCallInfo()
        f = unwrapconst(argtypes[1])
        isa(f, Core.Builtin) && return CallInfo[]
        return CallInfo[RTCallInfo(f, argtypes[2:end], rt, exct)]
    elseif info === false
        return CallInfo[]
    else
        @eval Main begin
            provider = $provider
            result = $result
            info = $info
            argtypes = $argtypes
            rt = $rt
        end
        error("unhandled `Compiler.CallInfo` of type $(typeof(info)); you may inspect `Main.provider|result|info|argtypes|rt`")
    end
end

unwrapType(@nospecialize t) = CC.isType(t) ? t.parameters[1] : t

ignorelhs(@nospecialize(x)) = isexpr(x, :(=)) ? last(x.args) : x
function is_call_expr(x::Expr, optimize::Bool)
    optimize && isexpr(x, :invoke) && return true
    return isexpr(ignorelhs(x), :call)
end

function callinfo(interp, sig, rt, max_methods=-1; world=get_world_counter())
    methds = Base._methods_by_ftype(sig, max_methods, world)
    methds isa Bool && return FailedCallInfo(sig, rt)
    length(methds) < 1 && return FailedCallInfo(sig, rt)
    callinfos = CallInfo[]
    for x in methds
        meth = x[3]
        atypes = x[1]
        sparams = x[2]
        if isdefined(meth, :generator) && !may_invoke_generator(meth, atypes, sparams)
            push!(callinfos, GeneratedCallInfo(sig, rt))
        else
            mi = specialize_method(meth, atypes, sparams)
            if mi !== nothing
                edge = generate_code_instance(interp, mi)
                push!(callinfos, EdgeCallInfo(edge, rt, Effects()))
            else
                push!(callinfos, FailedCallInfo(sig, rt))
            end
        end
    end

    @assert length(callinfos) != 0
    length(callinfos) == 1 && return first(callinfos)
    return MultiCallInfo(sig, rt, callinfos)
end

function add_sourceline!(locs::Vector{Tuple{Core.LineInfoNode,Int}}, src::Union{CodeInfo,IRCode}, stmtidx::Int, caller::MethodInstance)
    stack = IRShow.buildLineInfoNode(src.debuginfo, caller, stmtidx)
    for (i, di) in enumerate(stack)
        loc = Core.LineInfoNode(Main, di.method, di.file, di.line, zero(Int32))
        push!(locs, (loc, i-1))
    end
    return locs
end

function get_typed_sourcetext(mi::MethodInstance, src::CodeInfo, @nospecialize(rt); warn::Bool=true)
    tsn, mappings = TypedSyntax.tsn_and_mappings(mi, src, rt; warn, strip_macros=true)
    return truncate_if_defaultargs!(tsn, mappings, mi.def::Method)
end

function get_typed_sourcetext(mi::MethodInstance, ::IRCode, @nospecialize(rt); kwargs...)
    src, rt = TypedSyntax.code_typed1_tsn(mi)
    return get_typed_sourcetext(mi, src, rt; kwargs...)
end

# If we're filling in keyword args, just show the signature
truncate_if_defaultargs!(::Nothing, mappings, meth) = nothing, mappings
function truncate_if_defaultargs!(tsn, mappings, meth)
    is_kw_dispatch(meth) || meth.nargs < TypedSyntax.num_positional_args(tsn) || return tsn, mappings
    _, body = children(tsn)
    # eliminate the body node
    raw, bodyraw = tsn.raw, body.raw
    is_leaf(raw) && return tsn, mappings
    idx = findfirst(==(bodyraw), children(raw))
    empty!(mappings)
    idx === nothing && return tsn, mappings
    rawargs = children(raw)[1:idx-1]
    tsn.raw = typeof(raw)(raw.head, sum(nd -> nd.span, rawargs), rawargs)
    body.raw = typeof(bodyraw)(bodyraw.head, UInt32(0), nothing)
    body.children = nothing
    return tsn, mappings
end

is_kw_dispatch(meth::Method) = meth.name == :kwcall || Base.unwrap_unionall(meth.sig).parameters[1] === typeof(Core.kwcall) || !isempty(Base.kwarg_decl(meth))

function tag_runtime(node::TypedSyntaxNode, info)
    node.runtime = isa(info, RTCallInfo)
    return node
end
tag_runtime(node, info) = node
