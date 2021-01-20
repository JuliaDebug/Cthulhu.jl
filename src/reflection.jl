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

# https://github.com/JuliaLang/julia/pull/36318
_id(x) = x.id
if VERSION < v"1.6.0-DEV.272"
    const SlotOrArgument = Core.Slot
else
    _id(x::Core.Argument) = x.n
    const SlotOrArgument = Union{Core.Argument,Core.Slot}
end

if VERSION < v"1.2.0-DEV.354"
    function unwrapconst(a)
        if isa(a, Const)
            a = Core.Typeof(a.val)
        elseif isa(a, Core.Compiler.MaybeUndef)
            a = a.typ
        end
        return a
    end
else
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
end

transform(::Val, callsite) = callsite
function transform(::Val{:CuFunction}, callsite, callexpr, CI, mi, slottypes; params=nothing, kwargs...)
    sptypes = sptypes_from_meth_instance(mi)
    tt = argextype(callexpr.args[4], CI, sptypes, slottypes)
    ft = argextype(callexpr.args[3], CI, sptypes, slottypes)
    isa(tt, Const) || return callsite
    return Callsite(callsite.id, CuCallInfo(callinfo(Tuple{widenconst(ft), tt.val.parameters...}, Nothing, params=params)), callsite.head)
end

function find_callsites(CI::Core.CodeInfo, mi::Core.MethodInstance, slottypes; params=current_params(), multichoose::Bool=false, kwargs...)
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
        if isa(ft, Function)
            ft = typeof(ft)
        end
        argTs = argextype(c.args[arg_base + 3], CI, sptypes, slottypes)
        if isa(argTs, Const)
            sig = Tuple{widenconst(ft), argTs.val.parameters...}
            miinner = multichoose ? choose_method_instance(sig) : first_method_instance(sig)
            if miinner !== nothing
                callinfo = MICallInfo(miinner, rt.val)
            else
                callinfo = FailedCallInfo(sig, rt)
            end
        else
            callinfo = FailedCallInfo(Base.signature_type(ft, argTs), rt)
        end
        return Callsite(id, ReturnTypeCallInfo(callinfo), c.head)
    end
    function maybefixsplat(t, arg)
        if isa(arg, Core.SSAValue)
           arg = CI.ssavaluetypes[arg.id]
           if isa(arg, Core.Compiler.Const)
                arg = arg.val
           end
        end
        if isa(arg, Tuple)
            # Redo the type analysis in case there are any DataTypes in the tuple
            return Tuple{map(arg -> widenconst(argextype(arg, CI, sptypes, slottypes)), arg)...}
        end
        return t
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
                    callsite = Callsite(id, MICallInfo(c.args[1], rt), c.head)
                end
                mi = get_mi(callsite)
                if nameof(mi.def.module) === :CUDAnative && mi.def.name === :cufunction
                    callsite = transform(Val(:CuFunction), callsite, c, CI, mi, slottypes; params=params, kwargs...)
                elseif callsite.info isa MICallInfo
                    argtypes_ssa = map(c.args[3:end]) do a
                        if isa(a, Core.SSAValue)
                            a = CI.ssavaluetypes[a.id]
                            return unwrapconst(a)
                        elseif isa(a, SlotOrArgument)
                            a = slottypes[_id(a)]
                            return unwrapconst(a)
                        elseif isa(a, QuoteNode)
                            return Core.Typeof(a.value)
                        end
                        Core.Typeof(a)
                    end
                    sig_ssa = Tuple{Base.tuple_type_head(mi.def.sig), argtypes_ssa...}
                    if sig_ssa !== mi.def.sig
                        sig_callinfo = callinfo(sig_ssa, rt)
                        if get_mi(sig_callinfo) !== mi
                            callsite = Callsite(id, DeoptimizedCallInfo(sig_callinfo, callsite.info), c.head)
                        end
                    end
                end
            elseif c.head === :call
                rt = CI.ssavaluetypes[id]
                types = mapany(arg -> widenconst(argextype(arg, CI, sptypes, slottypes)), c.args)

                # Look through _apply
                ok = true
                while types[1] === typeof(Core._apply)
                    new_types = Any[types[2]]
                    for (i, t) in enumerate(types[3:end])
                        if i == 1 && t <: Tuple
                            t = maybefixsplat(t, c.args[3])
                        end
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
                        for i = 4:length(types)
                            t = types[i]
                            if i == 4 && t <: Tuple
                                t = maybefixsplat(t, c.args[4])
                            end
                            if t <: AbstractArray
                                if hasmethod(length, (Type{t},))
                                    for i = 1:length(t)
                                        push!(new_types, eltype(t))
                                    end
                                else
                                    push!(new_types, Vararg{eltype(t)})
                                    i == length(types) || (ok = false)
                                end
                                continue
                            end
                            if !(t <: Tuple) || t isa Union
                                ok = false
                                break
                            end
                            append!(new_types, Base.unwrap_unionall(t).parameters)
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
                    sigs = let types=types
                        mapany(ft-> Any[ft, types[2:end]...], fts)
                    end
                    cis = CallInfo[callinfo(Tuple{t...}, rt, params=params) for t in sigs]
                    callsite = Callsite(id, MultiCallInfo(Tuple{types...}, rt, cis), c.head)
                else
                    ft = Base.unwrap_unionall(types[1])
                    name = ft.name
                    ci = if nameof(name.module) === :CUDAnative && name.name === Symbol("#kw##cufunction")
                        ft = types[4]
                        # XXX: Simplify
                        tt = types[5].parameters[1].parameters
                        CuCallInfo(callinfo(Tuple{widenconst(ft), tt...}, Nothing, params=params))
                    else
                        callinfo(Tuple{types...}, rt, params=params)
                    end
                    callsite = Callsite(id, ci, c.head)
                end
            else c.head === :foreigncall
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
        frame === nothing && return (nothing, Any, Any[])
        if Compiler.typeinf(interp, frame) && run_optimizer
            oparams = Core.Compiler.OptimizationParams(interp)
            opt = Compiler.OptimizationState(frame, oparams, interp)
            Base.VERSION >= v"1.6.0-DEV.1410" ? Compiler.optimize(interp, opt, oparams, result.result) :
                                                Compiler.optimize(opt, oparams, result.result)
            opt.src.inferred = true
        end
        ccall(:jl_typeinf_end, Cvoid, ())
        frame.inferred || return (nothing, Any, Any[])
        return (frame.src, result.result, frame.slottypes)
    end
else
    function do_typeinf_slottypes(mi::Core.Compiler.MethodInstance, run_optimizer::Bool, params::Core.Compiler.Params)
        ccall(:jl_typeinf_begin, Cvoid, ())
        result = Core.Compiler.InferenceResult(mi)
        frame = Core.Compiler.InferenceState(result, false, params)
        frame === nothing && return (nothing, Any, Any[])
        if Compiler.typeinf(frame) && run_optimizer
            opt = Compiler.OptimizationState(frame)
            Compiler.optimize(opt, result.result)
            opt.src.inferred = true
        end
        ccall(:jl_typeinf_end, Cvoid, ())
        frame.inferred || return (nothing, Any, Any[])
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
