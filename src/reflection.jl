###
# Reflection tooling
##

using Base.Meta
using Base: may_invoke_generator

transform(::Val, callsite) = callsite
function transform(::Val{:CuFunction}, callsite, callexpr, CI, mi, slottypes; world=get_world_counter())
    sptypes = sptypes_from_meth_instance(mi)
    tt = argextype(callexpr.args[4], CI, sptypes, slottypes)
    ft = argextype(callexpr.args[3], CI, sptypes, slottypes)
    isa(tt, Const) || return callsite
    return Callsite(callsite.id, CuCallInfo(callinfo(Tuple{widenconst(ft), tt.val.parameters...}, Nothing; world)), callsite.head)
end

function find_callsites(interp::AbstractInterpreter, CI::Union{CodeInfo,IRCode},
                        stmt_infos::Union{Vector{CCCallInfo}, Nothing}, mi::MethodInstance,
                        slottypes::Vector{Any}, optimize::Bool=true, annotate_source::Bool=false,
                        pc2excts::Union{Nothing,PC2Excts}=nothing)
    sptypes = sptypes_from_meth_instance(mi)
    callsites, sourcenodes = Callsite[], Union{TypedSyntax.MaybeTypedSyntaxNode,Callsite}[]
    if isa(CI, IRCode)
        @static if VERSION < v"1.11.0-DEV.258"
            stmts = CI.stmts.inst
        else
            stmts = CI.stmts.stmt
        end
    else
        stmts = CI.code
    end
    nstmts = length(stmts)
    _, mappings = annotate_source ? get_typed_sourcetext(mi, CI, nothing; warn=false) : (nothing, nothing)

    for id = 1:nstmts
        stmt = stmts[id]
        isa(stmt, Expr) || continue
        callsite = nothing
        if stmt_infos !== nothing && is_call_expr(stmt, optimize)
            info = stmt_infos[id]
            if info !== nothing
                rt = ignorelimited(argextype(SSAValue(id), CI, sptypes, slottypes))
                # in unoptimized IR, there may be `slot = rhs` expressions, which `argextype` doesn't handle
                # so extract rhs for such an case
                local args = stmt.args
                if !optimize
                    args = (ignorelhs(stmt)::Expr).args
                end
                argtypes = Vector{Any}(undef, length(args))
                ft = ignorelimited(argextype(args[1], CI, sptypes, slottypes))
                f = CC.singleton_type(ft)
                f === Core.Intrinsics.llvmcall && continue
                f === Core.Intrinsics.cglobal && continue
                argtypes[1] = ft
                for i = 2:length(args)
                    t = argextype(args[i], CI, sptypes, slottypes)
                    argtypes[i] = ignorelimited(t)
                end
                exct = isnothing(pc2excts) ? nothing : get(pc2excts, id, nothing)
                callinfos = process_info(interp, info, argtypes, rt, optimize, exct)
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
                rt = argextype(SSAValue(id), CI, sptypes, slottypes)
                arg1 = args[1]::CodeInstance
                effects = get_effects(arg1) # TODO
                callsite = Callsite(id, EdgeCallInfo(arg1, rt, effects), head)
            elseif head === :foreigncall
                # special handling of jl_new_task
                length(args) > 0 || continue
                arg1 = args[1]
                if arg1 isa QuoteNode
                    cfunc = arg1.value
                    if cfunc === :jl_new_task
                        func = args[6]
                        ftype = widenconst(argextype(func, CI, sptypes, slottypes))
                        sig = Tuple{ftype}
                        callsite = Callsite(id, TaskCallInfo(callinfo(sig, nothing; world=get_inference_world(interp))), head)
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
                    callsite = transform(Val(:CuFunction), callsite, c, CI, ci.def, slottypes; world=get_inference_world(interp))
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

function process_const_info(interp::AbstractInterpreter, @nospecialize(thisinfo),
    argtypes::ArgTypes, @nospecialize(rt), @nospecialize(result), optimize::Bool,
    @nospecialize(exct))
    if isnothing(result)
        return thisinfo
    elseif (@static VERSION ≥ v"1.11.0-DEV.851" && true) && result isa CC.VolatileInferenceResult
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

function process_info(interp::AbstractInterpreter, @nospecialize(info::CCCallInfo),
                      argtypes::ArgTypes, @nospecialize(rt), optimize::Bool,
                      @nospecialize(exct))
    process_recursive(@nospecialize(newinfo)) = process_info(interp, newinfo, argtypes, rt, optimize, exct)

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
                effects = get_effects(edge)
                EdgeCallInfo(edge, rt, effects, exct)
            end
        end for edge in info.edges if edge !== nothing]
    elseif isa(info, UnionSplitInfo)
        @static if hasfield(UnionSplitInfo, :split)
            return mapreduce(process_recursive, vcat, info.split; init=CallInfo[])::Vector{CallInfo}
        else
            return mapreduce(process_recursive, vcat, info.matches; init=CallInfo[])::Vector{CallInfo}
        end
    elseif isa(info, UnionSplitApplyCallInfo)
        return mapreduce(process_recursive, vcat, info.infos; init=CallInfo[])::Vector{CallInfo}
    elseif isa(info, ApplyCallInfo)
        # XXX: This could probably use its own info. For now,
        # we ignore any implicit iterate calls.
        return process_recursive(info.call)
    elseif isa(info, ConstCallInfo)
        infos = process_recursive(info.call)
        @assert length(infos) == length(info.results)
        return CallInfo[let
            process_const_info(interp, infos[i], argtypes, rt, result, optimize, exct)
        end for (i, result) in enumerate(info.results)]
    elseif isa(info, CC.InvokeCallInfo)
        edge = info.edge
        if edge !== nothing
            effects = get_effects(edge)
            thisinfo = EdgeCallInfo(edge, rt, effects)
            innerinfo = process_const_info(interp, thisinfo, argtypes, rt, info.result, optimize, exct)
        else
            innerinfo = RTCallInfo(unwrapconst(argtypes[1]), argtypes[2:end], rt, exct)
        end
        info = InvokeCallInfo(innerinfo)
        return CallInfo[info]
    elseif isa(info, CC.OpaqueClosureCallInfo)
        edge = info.edge
        if edge !== nothing
            effects = get_effects(edge)
            thisinfo = EdgeCallInfo(edge, rt, effects)
            innerinfo = process_const_info(interp, thisinfo, argtypes, rt, info.result, optimize, exct)
        else
            innerinfo = RTCallInfo(unwrapconst(argtypes[1]), argtypes[2:end], rt, exct)
        end
        info = OCCallInfo(innerinfo)
        return CallInfo[info]
    elseif (@static VERSION ≥ v"1.12.0-DEV.1870" && true) && isa(info, CC.GlobalAccessInfo)
        return CallInfo[] # TODO return something informative here?
    elseif isa(info, CC.OpaqueClosureCreateInfo)
        # TODO: Add ability to descend into OCs at creation site
        return CallInfo[]
    elseif isa(info, CC.FinalizerInfo)
        # TODO: Add ability to descend into finalizers at creation site
        return CallInfo[]
    elseif isa(info, CC.ReturnTypeCallInfo)
        newargtypes = argtypes[2:end]
        callinfos = process_info(interp, info.info, newargtypes, unwrapType(widenconst(rt)), optimize, exct)
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
            interp = $interp
            info = $info
            argtypes = $argtypes
            rt = $rt
            optimize = $optimize
        end
        error("inspect `Main.interp|info|argtypes|rt|optimize`")
    end
end

unwrapType(@nospecialize t) = CC.isType(t) ? t.parameters[1] : t

ignorelhs(@nospecialize(x)) = isexpr(x, :(=)) ? last(x.args) : x
function is_call_expr(x::Expr, optimize::Bool)
    optimize && isexpr(x, :invoke) && return true
    return isexpr(ignorelhs(x), :call)
end

function dce!(ir::IRCode)
    ir = CC.compact!(ir, #=allow_cfg_transform=#true)
    ir = CC.compact!(ir, #=allow_cfg_transform=#true)
    return ir
end

function preprocess_ci!(ci::CodeInfo, mi::MethodInstance, optimize, config::CthulhuConfig)
    if optimize && config.dead_code_elimination
        argtypes = CC.matching_cache_argtypes(mi, nothing, false)[1]
        ir = CC.inflate_ir(ci, sptypes_from_meth_instance(mi), argtypes)
        ir = dce!(ir)
        ci = CC.replace_code_newstyle!(ci, ir)
    end
    return ci
end

function preprocess_ci!(ir::IRCode, _::MethodInstance, optimize::Bool, config::CthulhuConfig)
    if optimize && config.dead_code_elimination
        ir = dce!(ir)
    end
    return ir
end

function callinfo(sig, rt, max_methods=-1; world=get_world_counter())
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
                push!(callinfos, EdgeCallInfo(mi, rt, Effects()))
            else
                push!(callinfos, FailedCallInfo(sig, rt))
            end
        end
    end

    @assert length(callinfos) != 0
    length(callinfos) == 1 && return first(callinfos)
    return MultiCallInfo(sig, rt, callinfos)
end

function find_caller_of(interp::AbstractInterpreter, callee::Union{MethodInstance,Type}, caller::MethodInstance; allow_unspecialized::Bool=false)
    interp′ = CthulhuInterpreter(interp)
    do_typeinf!(interp′, caller)
    locs = Tuple{Core.LineInfoNode,Int}[]
    for optimize in (true, false)
        (; src, rt, infos, slottypes) = lookup(interp′, caller, optimize)
        src = preprocess_ci!(src, caller, optimize, CONFIG)
        callsites, _ = find_callsites(interp′, src, infos, caller, slottypes, optimize)
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

function add_sourceline!(locs::Vector{Tuple{Core.LineInfoNode,Int}}, src::Union{CodeInfo,IRCode}, stmtidx::Int, caller::MethodInstance)
    @static if VERSION ≥ v"1.12.0-DEV.173"
    stack = IRShow.buildLineInfoNode(src.debuginfo, caller, stmtidx)
    for (i, di) in enumerate(stack)
        loc = Core.LineInfoNode(Main, di.method, di.file, di.line, zero(Int32))
        push!(locs, (loc, i-1))
    end
    else # VERSION < v"1.12.0-DEV.173"
    if isa(src, IRCode)
        stack = IRShow.compute_loc_stack(src.linetable, src.stmts.line[stmtidx])
        for (i, idx) in enumerate(stack)
            line = src.linetable[idx]
            line.line == 0 && continue
            push!(locs, (src.linetable[idx], i-1))
        end
    else
        push!(locs, (src.linetable[src.codelocs[stmtidx]], 0))
    end
    end # @static if
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
    if (is_kw_dispatch(meth) || meth.nargs < TypedSyntax.num_positional_args(tsn))
        _, body = children(tsn)
        # eliminate the body node
        raw, bodyraw = tsn.raw, body.raw
        idx = findfirst(==(bodyraw), raw.args)
        if idx !== nothing
            rawargs = raw.args[1:idx-1]
            tsn.raw = typeof(raw)(raw.head, sum(nd -> nd.span, rawargs), rawargs)
            body.raw = typeof(bodyraw)(bodyraw.head, UInt32(0), ())
            cs = children(body)
            cs !== () && empty!(cs)
        end
        empty!(mappings)
    end
    return tsn, mappings
end

is_kw_dispatch(meth::Method) = meth.name == :kwcall || Base.unwrap_unionall(meth.sig).parameters[1] === typeof(Core.kwcall) || !isempty(Base.kwarg_decl(meth))

function tag_runtime(node::TypedSyntaxNode, info)
    node.runtime = isa(info, RTCallInfo)
    return node
end
tag_runtime(node, info) = node
