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

function find_callsites(CI::Union{Core.CodeInfo, IRCode}, stmt_info::Union{Vector, Nothing}, mi::Core.MethodInstance, slottypes; params=current_params(), multichoose::Bool=false, kwargs...)
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
                function process_info(info)
                    if isa(info, MethodMatchInfo)
                        if info.results === missing
                            return []
                        end

                        matches = info.results.matches
                        return map(match->MICallInfo(match, rt), matches)
                    elseif isa(info, UnionSplitInfo)
                        return mapreduce(process_info, vcat, info.matches)
                    elseif isa(info, UnionSplitApplyCallInfo)
                        return mapreduce(process_info, vcat, info.infos)
                    elseif isa(info, ApplyCallInfo)
                        # XXX: This could probably use its own info. For now,
                        # we ignore any implicit iterate calls.
                        return process_info(info.call)
                    elseif isa(info, ConstCallInfo)
                        if length(info.results) == 1
                            result = info.results[1]
                            @assert length(info.results) == 1
                            return [ConstPropCallInfo(MICallInfo(result.linfo, rt), result)]
                        else
                            vcat(process_info(info.call),
                                map(result->ConstPropCallInfo(MICallInfo(result.linfo, rt), result),
                                    filter(!isnothing, info.results)))
                        end
                    elseif isdefined(Compiler, :OpaqueClosureCallInfo) && isa(info, Compiler.OpaqueClosureCallInfo)
                        return [OCCallInfo(Core.Compiler.specialize_method(info.match), rt)]
                    elseif isdefined(Compiler, :OpaqueClosureCreateInfo) && isa(info, Compiler.OpaqueClosureCreateInfo)
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
                        types = mapany(arg -> widenconst(argextype(arg, CI, sptypes, slottypes)), c.args)
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
                        callsite = Callsite(id, TaskCallInfo(callinfo(sig, nothing, params=params)), c.head)
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
    argtypes = Core.Compiler.matching_cache_argtypes(mi, nothing)[1]
    ir = Compiler.inflate_ir(ci, sptypes_from_meth_instance(mi),
                             argtypes)
    dce!(ir, mi)
    Core.Compiler.replace_code_newstyle!(ci, ir, length(argtypes)-1)
end

function dce!(ir::IRCode, mi)
    compact = Core.Compiler.IncrementalCompact(ir, true)
    # Just run through the iterator without any processing
    Core.Compiler.foreach(x -> nothing, compact)
    ir = Core.Compiler.finish(compact)
end


function do_typeinf_slottypes(mi::Core.Compiler.MethodInstance, run_optimizer::Bool, interp::Core.Compiler.AbstractInterpreter)
    ccall(:jl_typeinf_begin, Cvoid, ())
    result = Core.Compiler.InferenceResult(mi)
    frame = Core.Compiler.InferenceState(result, false, interp)
    frame === nothing && return (nothing, Any, Any[])
    if Compiler.typeinf(interp, frame) && run_optimizer
        oparams = Core.Compiler.OptimizationParams(interp)
        opt = Compiler.OptimizationState(frame, oparams, interp)
        Compiler.optimize(interp, opt, oparams, result.result)
        opt.src.inferred = true
    end
    ccall(:jl_typeinf_end, Cvoid, ())
    frame.inferred || return (nothing, Any, Any[])
    return (frame.src, result.result, frame.slottypes)
end

function preprocess_ci!(ci::CodeInfo, mi, optimize, config::CthulhuConfig)
    if optimize && config.dead_code_elimination
        # if the optimizer hasn't run, the IR hasn't been converted
        # to SSA form yet and dce is not legal
        dce!(ci, mi)
        dce!(ci, mi)
    end
end

function preprocess_ci!(ir::IRCode, mi, optimize, config::CthulhuConfig)
    if optimize && config.dead_code_elimination
        # if the optimizer hasn't run, the IR hasn't been converted
        # to SSA form yet and dce is not legal
        ir = dce!(ir, mi)
        ir = dce!(ir, mi)
    end
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

function callinfo(sig, rt, max=-1; params=current_params())
    methds = Base._methods_by_ftype(sig, -1, params.world)
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
