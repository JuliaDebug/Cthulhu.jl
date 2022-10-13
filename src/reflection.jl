###
# Reflection tooling
##

using Base.Meta
import .CC: widenconst, argextype, Const, MethodMatchInfo,
    UnionSplitApplyCallInfo, UnionSplitInfo, ConstCallInfo,
    MethodResultPure, ApplyCallInfo,
    sptypes_from_meth_instance, argtypes_to_type
import Base: may_invoke_generator

transform(::Val, callsite) = callsite
function transform(::Val{:CuFunction}, callsite, callexpr, CI, mi, slottypes; world=get_world_counter())
    sptypes = sptypes_from_meth_instance(mi)
    tt = argextype(callexpr.args[4], CI, sptypes, slottypes)
    ft = argextype(callexpr.args[3], CI, sptypes, slottypes)
    isa(tt, Const) || return callsite
    return Callsite(callsite.id, CuCallInfo(callinfo(Tuple{widenconst(ft), tt.val.parameters...}, Nothing; world)), callsite.head)
end

function find_callsites(interp::AbstractInterpreter, CI::Union{Core.CodeInfo, IRCode},
                        stmt_infos::Union{Vector{CCCallInfo}, Nothing}, mi::Core.MethodInstance,
                        slottypes::Vector{Any}, optimize::Bool=true)
    sptypes = sptypes_from_meth_instance(mi)
    callsites = Callsite[]
    stmts = isa(CI, IRCode) ? CI.stmts.inst : CI.code
    nstmts = length(stmts)

    for id = 1:nstmts
        stmt = stmts[id]
        isa(stmt, Expr) || continue
        callsite = nothing
        if stmt_infos !== nothing && is_call_expr(stmt, optimize)
            info = stmt_infos[id]
            if info !== NoCallInfo()
                rt = ignorelimited(argextype(SSAValue(id), CI, sptypes, slottypes))
                # in unoptimized IR, there may be `slot = rhs` expressions, which `argextype` doesn't handle
                # so extract rhs for such an case
                local args = stmt.args
                if !optimize
                    args = (ignorelhs(stmt)::Expr).args
                end
                argtypes = mapany(function (@nospecialize(arg),)
                                      t = argextype(arg, CI, sptypes, slottypes)
                                      return ignorelimited(t)
                                  end, args)
                callinfos = process_info(interp, info, argtypes, rt, optimize)
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
                mi = args[1]::MethodInstance
                effects = get_effects(interp, mi, false)
                callsite = Callsite(id, MICallInfo(mi, rt, effects), head)
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
                        callsite = Callsite(id, TaskCallInfo(callinfo(sig, nothing; world=get_world_counter(interp))), head)
                    end
                end
            end
        end

        if callsite !== nothing
            info = callsite.info
            if info isa MICallInfo
                mi = get_mi(info)
                meth = mi.def
                if isa(meth, Method) && nameof(meth.module) === :CUDAnative && meth.name === :cufunction
                    callsite = transform(Val(:CuFunction), callsite, c, CI, mi, slottypes; world=get_world_counter(interp))
                end
            end

            push!(callsites, callsite)
        end
    end
    return callsites
end

function process_const_info(interp::AbstractInterpreter, @nospecialize(thisinfo),
    argtypes::ArgTypes, @nospecialize(rt), @nospecialize(result), optimize::Bool)
    is_cached(@nospecialize(key)) = can_descend(interp, key, optimize)

    if isnothing(result)
        return thisinfo
    elseif (@static VERSION ≥ v"1.9.0-DEV.409" && true) && isa(result, CC.ConcreteResult)
        linfo = result.mi
        effects = get_effects(result)
        mici = MICallInfo(linfo, rt, effects)
        return ConcreteCallInfo(mici, argtypes)
    elseif (@static VERSION ≥ v"1.9.0-DEV.409" && true) && isa(result, CC.ConstPropResult)
        result = result.result
        linfo = result.linfo
        effects = get_effects(result)
        mici = MICallInfo(linfo, rt, effects)
        return ConstPropCallInfo(is_cached(optimize ? linfo : result) ? mici : UncachedCallInfo(mici), result)
    elseif (@static isdefined(CC, :SemiConcreteResult) && true) && isa(result, CC.SemiConcreteResult)
        linfo = result.mi
        effects = get_effects(result)
        mici = MICallInfo(linfo, rt, effects)
        return SemiConcreteCallInfo(mici, result.ir)
    elseif (@static isdefined(CC, :ConstResult) && true) && isa(result, CC.ConstResult)
        linfo = result.mi
        effects = get_effects(result)
        mici = MICallInfo(linfo, rt, effects)
        return ConcreteCallInfo(mici, argtypes)
    else
        @assert isa(result, CC.InferenceResult)
        linfo = result.linfo
        effects = get_effects(result)
        mici = MICallInfo(linfo, rt, effects)
        return ConstPropCallInfo(is_cached(optimize ? linfo : result) ? mici : UncachedCallInfo(mici), result)
    end
end

function process_info(interp::AbstractInterpreter, @nospecialize(info::CCCallInfo), argtypes::ArgTypes, @nospecialize(rt), optimize::Bool)
    is_cached(@nospecialize(key)) = can_descend(interp, key, optimize)
    process_recursive(@nospecialize(newinfo)) = process_info(interp, newinfo, argtypes, rt, optimize)

    if isa(info, MethodResultPure)
        if isa(info.info, CC.ReturnTypeCallInfo)
            # xref: https://github.com/JuliaLang/julia/pull/45299#discussion_r871939049
            info = info.info # cascade to the special handling below
        else
            return Any[PureCallInfo(argtypes, rt)]
        end
    end
    if isa(info, MethodMatchInfo)
        if info.results === missing
            return []
        end
        matches = info.results.matches
        return mapany(matches) do match::Core.MethodMatch
            mi = specialize_method(match)
            effects = get_effects(interp, mi, false)
            mici = MICallInfo(mi, rt, effects)
            return is_cached(mi) ? mici : UncachedCallInfo(mici)
        end
    elseif isa(info, UnionSplitInfo)
        return mapreduce(process_recursive, vcat, info.matches; init=[])::Vector{Any}
    elseif isa(info, UnionSplitApplyCallInfo)
        return mapreduce(process_recursive, vcat, info.infos; init=[])::Vector{Any}
    elseif isa(info, ApplyCallInfo)
        # XXX: This could probably use its own info. For now,
        # we ignore any implicit iterate calls.
        return process_recursive(info.call)
    elseif isa(info, ConstCallInfo)
        infos = process_recursive(info.call)
        @assert length(infos) == length(info.results)
        return mapany(enumerate(info.results)) do (i, result)
            process_const_info(interp, infos[i], argtypes, rt, result, optimize)
        end
    elseif (@static isdefined(CC, :InvokeCallInfo) && true) && isa(info, CC.InvokeCallInfo)
        mi = specialize_method(info.match; preexisting=true)
        effects = get_effects(interp, mi, false)
        thisinfo = MICallInfo(mi, rt, effects)
        @static if hasfield(CC.InvokeCallInfo, :result)
            innerinfo = process_const_info(interp, thisinfo, argtypes, rt, info.result, optimize)
            info = InvokeCallInfo(innerinfo)
        else
            info = InvokeCallInfo(thisinfo)
        end
        return Any[info]
    elseif (@static isdefined(CC, :OpaqueClosureCallInfo) && true) && isa(info, CC.OpaqueClosureCallInfo)
        mi = specialize_method(info.match; preexisting=true)
        effects = get_effects(interp, mi, false)
        thisinfo = MICallInfo(mi, rt, effects)
        @static if hasfield(CC.OpaqueClosureCallInfo, :result)
            innerinfo = process_const_info(interp, thisinfo, argtypes, rt, info.result, optimize)
            info = OCCallInfo(innerinfo)
        else
            info = OCCallInfo(thisinfo)
        end
        return Any[info]
    elseif (@static isdefined(CC, :OpaqueClosureCreateInfo) && true) && isa(info, CC.OpaqueClosureCreateInfo)
        # TODO: Add ability to descend into OCs at creation site
        return []
    elseif (@static isdefined(CC, :FinalizerInfo) && true) && isa(info, CC.FinalizerInfo)
        # TODO: Add ability to descend into finalizers at creation site
        return []
    elseif isa(info, CC.ReturnTypeCallInfo)
        newargtypes = argtypes[2:end]
        callinfos = process_info(interp, info.info, newargtypes, unwrapType(widenconst(rt)), optimize)
        if length(callinfos) == 1
            vmi = only(callinfos)
        else
            @assert isempty(callinfos)
            argt = unwrapType(widenconst(newargtypes[2]))::DataType
            sig = Tuple{widenconst(newargtypes[1]), argt.parameters...}
            vmi = FailedCallInfo(sig, Union{})
        end
        return Any[ReturnTypeCallInfo(vmi)]
    elseif info == NoCallInfo() || info === false
        return []
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
    ir = Core.Compiler.compact!(ir, #=allow_cfg_transform=#true)
    ir = Core.Compiler.compact!(ir, #=allow_cfg_transform=#true)
    return ir
end

function preprocess_ci!(ci::CodeInfo, mi::MethodInstance, optimize, config::CthulhuConfig)
    if optimize && config.dead_code_elimination
        @static if VERSION >= v"1.7.0-DEV.705"
            argtypes = CC.matching_cache_argtypes(mi, nothing, false)[1]
        else
            argtypes = CC.matching_cache_argtypes(mi, nothing)[1]
        end
        ir = CC.inflate_ir(ci, sptypes_from_meth_instance(mi), argtypes)
        ir = dce!(ir)
        ci = CC.replace_code_newstyle!(ci, ir, length(argtypes)-1)
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
                push!(callinfos, MICallInfo(mi, rt, Effects()))
            else
                push!(callinfos, FailedCallInfo(sig, rt))
            end
        end
    end

    @assert length(callinfos) != 0
    length(callinfos) == 1 && return first(callinfos)
    return MultiCallInfo(sig, rt, callinfos)
end

function find_caller_of(interp::AbstractInterpreter, callee::MethodInstance, caller::MethodInstance; allow_unspecialized::Bool=false)
    interp′ = CthulhuInterpreter(interp)
    do_typeinf!(interp′, caller)
    locs = Tuple{Core.LineInfoNode,Int}[]
    for optimize in (true, false)
        (; src, rt, infos, slottypes) = lookup(interp′, caller, optimize)
        src = preprocess_ci!(src, caller, optimize, CONFIG)
        callsites = find_callsites(interp′, src, infos, caller, slottypes, optimize)
        callsites = allow_unspecialized ? filter(cs->maybe_callsite(cs, callee), callsites) :
                                          filter(cs->is_callsite(cs, callee), callsites)
        foreach(cs -> add_sourceline!(locs, src, cs.id), callsites)
    end
    # Consolidate by method, but preserve the order
    prlookup = Dict{Tuple{Symbol,Symbol},Int}()
    ulocs = Pair{Tuple{Symbol,Symbol,Int},Vector{Int}}[]
    if !isempty(locs)
        for (loc, depth) in locs
            idx = get(prlookup, (loc.method, loc.file), nothing)
            if idx === nothing
                push!(ulocs, (loc.method, loc.file, depth) => Int[])
                prlookup[(loc.method, loc.file)] = idx = length(ulocs)
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

function add_sourceline!(locs, CI, stmtidx::Int)
    if isa(CI, IRCode)
        stack = Base.IRShow.compute_loc_stack(CI.linetable, CI.stmts.line[stmtidx])
        for (i, idx) in enumerate(stack)
            line = CI.linetable[idx]
            line.line == 0 && continue
            push!(locs, (CI.linetable[idx], i-1))
        end
    else
        push!(locs, (CI.linetable[CI.codelocs[stmtidx]], 0))
    end
    return locs
end
