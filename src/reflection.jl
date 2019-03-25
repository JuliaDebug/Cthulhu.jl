###
# Reflection tooling
##

using Base.Meta
using .Compiler: widenconst, argextype, Const

if VERSION >= v"1.2.0-DEV.249"
    sptypes_from_meth_instance(mi) = Core.Compiler.sptypes_from_meth_instance(mi)
else
    sptypes_from_meth_instance(mi) = Core.Compiler.spvals_from_meth_instance(mi)
end

if VERSION >= v"1.1.0-DEV.157"
    const is_return_type = Core.Compiler.is_return_type
else
    is_return_type(f) = f === Core.Compiler.return_type
end

if VERSION < v"1.2.0-DEV.573"
    code_for_method(method, metharg, methsp, world, force=false) = Core.Compiler.code_for_method(method, metharg, methsp, world, force)
#    get_world() = typemax(UInt) 
else
    code_for_method(method, metharg, methsp, world, force=false) = Core.Compiler.specialize_method(method, metharg, methsp, force)
#    get_world() = Core.Compiler.get_world_counter()
end

function find_callsites(CI, mi, slottypes; params=current_params(), kwargs...)
    sptypes = sptypes_from_meth_instance(mi)
    callsites = Callsite[]

    function process_return_type(id, c, rt)
        is_call = isexpr(c, :call)
        arg_base = is_call ? 0 : 1
        length(c.args) == (arg_base + 3) || return nothing
        ft = argextype(c.args[arg_base + 2], CI, sptypes, slottypes)
        argTs = argextype(c.args[arg_base + 3], CI, sptypes, slottypes)
        isa(argTs, Const) || return nothing
        mi = first_method_instance(Tuple{widenconst(ft), argTs.val.parameters...})
        return Callsite(id, ReturnTypeCallInfo(MICallInfo(mi, rt.val)))
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

                # Filter out builtin functions and intrinsic function
                if types[1] <: Core.Builtin || types[1] <: Core.IntrinsicFunction
                    continue
                end

                if isdefined(types[1], :instance) && is_return_type(types[1].instance)
                    callsite = process_return_type(id, c, rt)
                else
                    callsite = Callsite(id, callinfo(Tuple{types...}, rt, params=params))
                end
            else c.head === :foreigncall
                # special handling of jl_threading_run
                length(c.args) > 0 || continue
                if c.args[1] isa QuoteNode && c.args[1].value === :jl_threading_run
                    func = c.args[7]
                    ftype = widenconst(argextype(func, CI, sptypes, slottypes))
                    callsite = Callsite(id, callinfo(ftype, nothing, params=params))
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

function preprocess_ci!(ci, mi, optimize)
    if optimize
        # if the optimizer hasn't run, the IR hasn't been converted
        # to SSA form yet and dce is not legal
        dce!(ci, mi)
        dce!(ci, mi)
    end
end

if :trace_inference_limits in fieldnames(Core.Compiler.Params)
    current_params() = Core.Compiler.CustomParams(ccall(:jl_get_world_counter, UInt, ()); trace_inference_limits=true)
else
    current_params() = Core.Compiler.Params(ccall(:jl_get_world_counter, UInt, ()))
end

function callinfo(sig, rt; params=current_params())
    methds = Base._methods_by_ftype(Tuple{sig.parameters...}, 1, params.world)
    (methds === false || length(methds) < 1) && return FailedCallInfo(sig, rt)
    callinfos = CallInfo[]
    for x in methds
        meth = x[3]
        atypes = x[1]
        sparams = x[2]
        if isdefined(meth, :generator) && !Base.may_invoke_generator(meth, atypes, sparams)
            push!(callinfos, GeneratedCallInfo(sig, rt))
        else
            mi = code_for_method(meth, atypes, sparams, params.world)
            push!(callinfos, MICallInfo(mi, rt)) 
        end
    end
    
    @assert length(callinfos) != 0
    length(callinfos) == 1 && return first(callinfos)
    return MultiCallInfo(sig, rt, callinfos)
end

function first_method_instance(F, TT; params=current_params())
    sig = Tuple{typeof(F), TT.parameters...}
    first_method_instance(sig; params=params)
end

function first_method_instance(sig; params=current_params())
    methds = Base._methods_by_ftype(sig, 1, params.world)
    (methds === false || length(methds) < 1) && return nothing
    x = methds[1]
    meth = x[3]
    if isdefined(meth, :generator) && !isdispatchtuple(Tuple{sig.parameters[2:end]...})
        return nothing
    end
    mi = code_for_method(meth, sig, x[2], params.world)
end
