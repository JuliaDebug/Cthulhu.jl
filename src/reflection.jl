###
# Reflection tooling
##

using Base.Meta
using .Compiler: widenconst, argextype, Const

if VERSION >= v"1.1.0-DEV.157"
    const is_return_type = Core.Compiler.is_return_type
else
    is_return_type(f) = f === Core.Compiler.return_type
end

if VERSION >= v"1.2.0-DEV.249"
    const sptypes_from_meth_instance = Core.Compiler.sptypes_from_meth_instance
else
    sptypes_from_meth_instance(mi) = Core.Compiler.spvals_from_meth_instance(mi)
end

if VERSION >= v"1.2.0-DEV.320"
    const may_invoke_generator = Base.may_invoke_generator
else
    may_invoke_generator(meth, @nospecialize(atypes), sparams) = isdispatchtuple(atypes)
end

if VERSION < v"1.2.0-DEV.573"
    code_for_method(method, metharg, methsp, world, force=false) = Core.Compiler.code_for_method(method, metharg, methsp, world, force)
else
    code_for_method(method, metharg, methsp, world, force=false) = Core.Compiler.specialize_method(method, metharg, methsp, force)
end

transform(::Val, callsite) = callsite
function transform(::Val{:CuFunction}, callsite, callexpr, CI, mi, slottypes; params=nothing, kwargs...)
    sptypes = sptypes_from_meth_instance(mi)
    tt = argextype(callexpr.args[4], CI, sptypes, slottypes)
    ft = argextype(callexpr.args[3], CI, sptypes, slottypes)
    isa(tt, Const) || return callsite
    return Callsite(callsite.id, CuCallInfo(callinfo(Tuple{widenconst(ft), tt.val.parameters...}, Nothing, params=params)))
end

function find_callsites(CI, mi, slottypes; params=current_params(), kwargs...)
    sptypes = sptypes_from_meth_instance(mi)
    callsites = Callsite[]

    function process_return_type(id, c, rt)
        callinfo = nothing
        is_call = isexpr(c, :call)
        arg_base = is_call ? 0 : 1
        length(c.args) == (arg_base + 3) || return nothing
        ft = argextype(c.args[arg_base + 2], CI, sptypes, slottypes)
        if isa(ft, Const)
            ft = ft.val
        end
        argTs = argextype(c.args[arg_base + 3], CI, sptypes, slottypes)
        if isa(argTs, Const)
            sig = Tuple{widenconst(ft), argTs.val.parameters...}
            mi = first_method_instance(sig)
            if mi !== nothing
                callinfo = MICallInfo(mi, rt.val)
            else
                callinfo = FailedCallInfo(sig, rt)
            end
        else
            callinfo = FailedCallInfo(Base.signature_type(ft, argTs), rt)
        end
        return Callsite(id, ReturnTypeCallInfo(callinfo))
    end

    for (id, c) in enumerate(CI.code)
        if c isa Expr
            callsite = nothing
            if c.head === :(=)
                c = c.args[2]
                (c isa Expr) || continue
            end
            if c.head === :invoke
                rt = CI.ssavaluetypes[id]
                at = argextype(c.args[2], CI, sptypes, slottypes)
                if isa(at, Const) && is_return_type(at.val)
                    callsite = process_return_type(id, c, rt)
                else
                    callsite = Callsite(id, MICallInfo(c.args[1], rt))
                end
                mi = get_mi(callsite)
                if nameof(mi.def.module) == :CUDAnative && mi.def.name == :cufunction
                    callsite = transform(Val(:CuFunction), callsite, c, CI, mi, slottypes; params=params, kwargs...)
                end
            elseif c.head === :call
                rt = CI.ssavaluetypes[id]
                types = map(arg -> widenconst(argextype(arg, CI, sptypes, slottypes)), c.args)

                # Look through _apply
                ok = true
                while types[1] === typeof(Core._apply)
                    new_types = Any[types[2]]
                    for t in types[3:end]
                        if !(t <: Tuple) || t isa Union
                            ok = false
                            break
                        end
                        append!(new_types, t.parameters)
                    end
                    ok || break
                    types = new_types
                end
                ok || continue

                @static if VERSION >= v"1.4.0-DEV.304"
                    # Look through _apply_iterate
                    if types[1] === typeof(Core._apply_iterate)
                        ok = true
                        new_types = Any[types[3]]
                        for t in types[4:end]
                            if !(t <: Tuple) || t isa Union
                                ok = false
                                break
                            end
                            append!(new_types, t.parameters)
                        end
                        ok || continue
                        types = new_types
                    end
                end

                # Filter out builtin functions and intrinsic function
                if types[1] <: Core.Builtin || types[1] <: Core.IntrinsicFunction
                    continue
                end

                if isdefined(types[1], :instance) && is_return_type(types[1].instance)
                    callsite = process_return_type(id, c, rt)
                elseif types[1] isa Union
                    # Union{typeof(sin), typeof(cos)}
                    fts = Any[]
                    function thatcher(u)
                        if u isa Union
                            thatcher(u.a)
                            thatcher(u.b)
                        else
                            push!(fts, u)
                        end
                    end
                    thatcher(types[1])
                    sigs = map(ft-> [ft, types[2:end]...], fts)
                    cis = map(types -> callinfo(Tuple{types...}, rt, params=params), sigs)
                    callsite = Callsite(id, MultiCallInfo(Tuple{types...}, rt, cis))
                else
                    ft = Base.unwrap_unionall(types[1])
                    name = ft.name
                    ci = if nameof(name.module) == :CUDAnative && name.name == Symbol("#kw##cufunction")
                        ft = types[4]
                        # XXX: Simplify
                        tt = types[5].parameters[1].parameters
                        CuCallInfo(callinfo(Tuple{widenconst(ft), tt...}, Nothing, params=params))
                    else
                        callinfo(Tuple{types...}, rt, params=params)
                    end
                    callsite = Callsite(id, ci)
                end
            else c.head === :foreigncall
                # special handling of jl_new_task
                length(c.args) > 0 || continue
                if c.args[1] isa QuoteNode
                    cfunc = c.args[1].value
                    if cfunc === :jl_new_task
                        func = c.args[7]
                        ftype = widenconst(argextype(func, CI, sptypes, slottypes))
                        callsite = Callsite(id, TaskCallInfo(callinfo(ftype, nothing, params=params)))
                    end
                end
            end

            if callsite !== nothing
                push!(callsites, callsite)
            end
        end
    end
    return callsites
end

if VERSION >= v"1.1.0-DEV.215"
function dce!(ci, mi)
    argtypes = Core.Compiler.matching_cache_argtypes(mi, nothing)[1]
    ir = Compiler.inflate_ir(ci, sptypes_from_meth_instance(mi),
                             argtypes)
    compact = Core.Compiler.IncrementalCompact(ir, true)
    # Just run through the iterator without any processing
    Core.Compiler.foreach(x -> nothing, compact)
    ir = Core.Compiler.finish(compact)
    Core.Compiler.replace_code_newstyle!(ci, ir, length(argtypes)-1)
end
else
function dce!(ci, mi)
end
end

if isdefined(Core.Compiler, :AbstractInterpreter)
    function do_typeinf_slottypes(mi::Core.Compiler.MethodInstance, run_optimizer::Bool, interp::Core.Compiler.AbstractInterpreter)
        ccall(:jl_typeinf_begin, Cvoid, ())
        result = Core.Compiler.InferenceResult(mi)
        frame = Core.Compiler.InferenceState(result, false, interp)
        frame === nothing && return (nothing, Any)
        if Compiler.typeinf(interp, frame) && run_optimizer
            oparams = Core.Compiler.OptimizationParams(interp)
            opt = Compiler.OptimizationState(frame, oparams, interp)
            Compiler.optimize(opt, oparams, result.result)
            opt.src.inferred = true
        end
        ccall(:jl_typeinf_end, Cvoid, ())
        frame.inferred || return (nothing, Any)
        return (frame.src, result.result, frame.slottypes)
    end
else
    function do_typeinf_slottypes(mi::Core.Compiler.MethodInstance, run_optimizer::Bool, params::Core.Compiler.Params)
        ccall(:jl_typeinf_begin, Cvoid, ())
        result = Core.Compiler.InferenceResult(mi)
        frame = Core.Compiler.InferenceState(result, false, params)
        frame === nothing && return (nothing, Any)
        if Compiler.typeinf(frame) && run_optimizer
            opt = Compiler.OptimizationState(frame)
            Compiler.optimize(opt, result.result)
            opt.src.inferred = true
        end
        ccall(:jl_typeinf_end, Cvoid, ())
        frame.inferred || return (nothing, Any)
        return (frame.src, result.result, frame.slottypes)
    end
end

function preprocess_ci!(ci, mi, optimize, config::CthulhuConfig)
    if optimize && config.dead_code_elimination
        # if the optimizer hasn't run, the IR hasn't been converted
        # to SSA form yet and dce is not legal
        dce!(ci, mi)
        dce!(ci, mi)
    end
end

if isdefined(Core.Compiler, :AbstractInterpreter)
    const CompilerParams = Core.Compiler.NativeInterpreter
    current_params() = CompilerParams()
else
    if :trace_inference_limits in fieldnames(Core.Compiler.Params)
        const CompilerParams = Core.Compiler.CustomParams
        current_params() = CompilerParams(ccall(:jl_get_world_counter, UInt, ()); trace_inference_limits=true)
    else
        const CompilerParams = Core.Compiler.Params
        current_params() = CompilerParams(ccall(:jl_get_world_counter, UInt, ()))
    end
end

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
