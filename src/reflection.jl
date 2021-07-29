###
# Reflection tooling
##

using Base.Meta
using .Compiler: widenconst, argextype, Const, MethodMatchInfo,
    UnionSplitApplyCallInfo, UnionSplitInfo, ConstCallInfo,
    MethodResultPure, ApplyCallInfo

const is_return_type = Core.Compiler.is_return_type
const sptypes_from_meth_instance = Core.Compiler.sptypes_from_meth_instance
const may_invoke_generator = Base.may_invoke_generator
code_for_method(method, metharg, methsp, world, force=false) = Core.Compiler.specialize_method(method, metharg, methsp, force)

# https://github.com/JuliaLang/julia/pull/36318
_id(x) = x.id
_id(x::Core.Argument) = x.n
const SlotOrArgument = Union{Core.Argument,Core.Slot}

function unwrapconst(a)
    if isa(a, Const)
        a = Core.Typeof(a.val)
    elseif isa(a, Core.Compiler.PartialStruct)
        a = a.typ
    elseif isa(a, Core.Compiler.MaybeUndef)
        a = a.typ
    end
    return a
end

transform(::Val, callsite) = callsite
function transform(::Val{:CuFunction}, callsite, callexpr, CI, mi, slottypes; params=nothing, kwargs...)
    sptypes = sptypes_from_meth_instance(mi)
    tt = argextype(callexpr.args[4], CI, sptypes, slottypes)
    ft = argextype(callexpr.args[3], CI, sptypes, slottypes)
    isa(tt, Const) || return callsite
    return Callsite(callsite.id, CuCallInfo(callinfo(Tuple{widenconst(ft), tt.val.parameters...}, Nothing, params=params)), callsite.head)
end

function find_callsites(interp::CthulhuInterpreter, CI::Union{Core.CodeInfo, IRCode},
                        stmt_info::Union{Vector, Nothing}, mi::Core.MethodInstance,
                        slottypes::Vector{Any}, optimize::Bool=true;
                        params=current_params(), kwargs...)
    sptypes = sptypes_from_meth_instance(mi)
    callsites = Callsite[]

    for (id, c) in enumerate(isa(CI, IRCode) ? CI.stmts.inst : CI.code)
        callsite = nothing
        isa(c, Expr) || continue
        if stmt_info !== nothing
            info = stmt_info[id]
            if info !== nothing
                rt = argextype(SSAValue(id), CI, sptypes, slottypes)
                was_return_type = false
                if isa(info, MethodResultPure)
                    # TODO: We could annotate this in the UI
                    continue
                end
                if isa(info, Core.Compiler.ReturnTypeCallInfo)
                    info = info.info
                    was_return_type = true
                end
                is_cached(key) = haskey(optimize ? interp.opt : interp.unopt, key)
                function process_info(info)
                    if isa(info, MethodMatchInfo)
                        if info.results === missing
                            return []
                        end

                        matches = info.results.matches
                        return map(matches) do match
                            mi = Core.Compiler.specialize_method(match)
                            mici = MICallInfo(mi, rt)
                            return is_cached(mi) ? mici : UncachedCallInfo(mici)
                        end
                    elseif isa(info, UnionSplitInfo)
                        return mapreduce(process_info, vcat, info.matches)
                    elseif isa(info, UnionSplitApplyCallInfo)
                        return mapreduce(process_info, vcat, info.infos; init=Core.Compiler.ApplyCallInfo[])
                    elseif isa(info, ApplyCallInfo)
                        # XXX: This could probably use its own info. For now,
                        # we ignore any implicit iterate calls.
                        return process_info(info.call)
                    elseif isa(info, ConstCallInfo)
                        infos = process_info(info.call)
                        @assert length(infos) == length(info.results)
                        return map(enumerate(info.results)) do (i, result)
                            if isnothing(result)
                                infos[i]
                            else
                                linfo = result.linfo
                                mici = MICallInfo(linfo, rt)
                                ConstPropCallInfo(is_cached(optimize ? linfo : result) ? mici : UncachedCallInfo(mici), result)
                            end
                        end
                    elseif (@static isdefined(Compiler, :InvokeCallInfo) && true) && isa(info, Compiler.InvokeCallInfo)
                        return [InvokeCallInfo(Core.Compiler.specialize_method(info.match), rt)]
                    elseif (@static isdefined(Compiler, :OpaqueClosureCallInfo) && true) && isa(info, Compiler.OpaqueClosureCallInfo)
                        return [OCCallInfo(Core.Compiler.specialize_method(info.match), rt)]
                    elseif (@static isdefined(Compiler, :OpaqueClosureCreateInfo) && true) && isa(info, Compiler.OpaqueClosureCreateInfo)
                        # TODO: Add ability to descend into OCs at creation site
                        return []
                    elseif info == false
                        return []
                    else
                        @show CI
                        @show c
                        @show info
                        error()
                    end
                end
                callinfos = process_info(info)
                if !was_return_type && isempty(callinfos)
                    continue
                end
                callsite = let
                    if length(callinfos) == 1
                        callinfo = callinfos[1]
                    else
                        # in unoptimized IR, there may be `slot = rhs` expressions, which `argextype` doesn't handle
                        # so extract rhs for such an case
                        args = c.args
                        if !optimize
                            if isexpr(c, :(=))
                                args = c.args[2].args
                            end
                        end
                        types = mapany(arg -> widenconst(argextype(arg, CI, sptypes, slottypes)), args)
                        callinfo = MultiCallInfo(Core.Compiler.argtypes_to_type(types), rt, callinfos)
                    end
                    if was_return_type
                        callinfo = ReturnTypeCallInfo(callinfo)
                    end
                    Callsite(id, callinfo, c.head)
                end
            end
        end

        if callsite === nothing && c isa Expr
            if c.head === :(=)
                c = c.args[2]
                (c isa Expr) || continue
            end
            if c.head === :invoke
                rt = argextype(SSAValue(id), CI, sptypes, slottypes)
                at = argextype(c.args[2], CI, sptypes, slottypes)
                if isa(at, Const) && is_return_type(at.val)
                    callsite = process_return_type(id, c, rt)
                else
                    callsite = Callsite(id, MICallInfo(c.args[1], rt), c.head)
                end
            elseif c.head === :foreigncall
                # special handling of jl_new_task
                length(c.args) > 0 || continue
                if c.args[1] isa QuoteNode
                    cfunc = c.args[1].value
                    if cfunc === :jl_new_task
                        func = c.args[6]
                        ftype = widenconst(argextype(func, CI, sptypes, slottypes))
                        sig = Tuple{ftype}
                        callsite = Callsite(id, TaskCallInfo(callinfo(sig, nothing; params)), c.head)
                    end
                end
            end
        end

        if callsite !== nothing
            if callsite.info isa MICallInfo
                mi = get_mi(callsite)
                if nameof(mi.def.module) === :CUDAnative && mi.def.name === :cufunction
                    callsite = transform(Val(:CuFunction), callsite, c, CI, mi, slottypes; params=params, kwargs...)
                end
            end

            push!(callsites, callsite)
        end
    end
    return callsites
end

function dce!(ci, mi)
    if VERSION >= v"1.7.0-DEV.705"
        argtypes = Core.Compiler.matching_cache_argtypes(mi, nothing, false)[1]
    else
        argtypes = Core.Compiler.matching_cache_argtypes(mi, nothing)[1]
    end
    ir = Compiler.inflate_ir(ci, sptypes_from_meth_instance(mi),
                             argtypes)
    ir = dce!(ir, mi)
    Core.Compiler.replace_code_newstyle!(ci, ir, length(argtypes)-1)
end

function dce!(ir::IRCode, mi)
    compact = Core.Compiler.IncrementalCompact(ir, true)
    # Just run through the iterator without any processing
    Core.Compiler.foreach(x -> nothing, compact)
    ir = Core.Compiler.finish(compact)
end

function preprocess_ci!(ci::CodeInfo, mi, optimize, config::CthulhuConfig)
    if optimize && config.dead_code_elimination
        # if the optimizer hasn't run, the IR hasn't been converted
        # to SSA form yet and dce is not legal
        dce!(ci, mi)
        dce!(ci, mi)
    end
    return ci
end

function preprocess_ci!(ir::IRCode, mi, optimize, config::CthulhuConfig)
    if optimize && config.dead_code_elimination
        # if the optimizer hasn't run, the IR hasn't been converted
        # to SSA form yet and dce is not legal
        ir = dce!(ir, mi)
        ir = dce!(ir, mi)
    end
    return ir
end

const CompilerParams = Core.Compiler.NativeInterpreter
current_params() = CompilerParams()

function first_method_instance(@nospecialize(F), @nospecialize(TT); params=current_params())
    sig = Base.signature_type(F, TT)
    first_method_instance(sig; params=params)
end

function first_method_instance(@nospecialize(sig); params=current_params())
    ci = callinfo(sig, Any, 1, params=params)
    if ci isa Union{GeneratedCallInfo, FailedCallInfo, MultiCallInfo}
        if ci isa MultiCallInfo
            @warn "expected single CallInfo got multiple" ci
        end
        return nothing
    else
        @assert ci isa MICallInfo
        return get_mi(ci)
    end
end

function choose_method_instance(@nospecialize(F), @nospecialize(TT); params=current_params())
    sig = Base.signature_type(F, TT)
    choose_method_instance(sig; params=params)
end

function callinfo(sig, rt, max_methods=-1; params=current_params())
    methds = Base._methods_by_ftype(sig, max_methods, params.world)
    (methds === false || length(methds) < 1) && return FailedCallInfo(sig, rt)
    callinfos = CallInfo[]
    for x in methds
        meth = x[3]
        atypes = x[1]
        sparams = x[2]
        if isdefined(meth, :generator) && !may_invoke_generator(meth, atypes, sparams)
            push!(callinfos, GeneratedCallInfo(sig, rt))
        else
            mi = code_for_method(meth, atypes, sparams, params.world)
            if mi !== nothing
                push!(callinfos, MICallInfo(mi, rt))
            else
                push!(callinfos, FailedCallInfo(sig, rt))
            end
        end
    end

    @assert length(callinfos) != 0
    length(callinfos) == 1 && return first(callinfos)
    return MultiCallInfo(sig, rt, callinfos)
end

function find_caller_of(callee::MethodInstance, caller::MethodInstance)
    interp = CthulhuInterpreter()
    do_typeinf!(interp, caller)
    params = current_params()
    locs = Tuple{Core.LineInfoNode,Int}[]
    for optimize in (true, false)
        (CI, rt, infos, slottypes) = lookup(interp, caller, optimize)
        CI = preprocess_ci!(CI, caller, optimize, CONFIG)
        callsites = find_callsites(interp, CI, infos, caller, slottypes, optimize; params=params)
        callsites = filter(cs->is_callsite(cs, callee), callsites)
        foreach(cs -> add_sourceline!(locs, CI, cs.id), callsites)
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
