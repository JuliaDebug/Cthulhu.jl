###
# Reflection tooling
##

unwrap_type(T) = T
unwrap_type(T::Core.Compiler.Const) = typeof(T.val)

if VERSION >= v"1.2.0-DEV.249"
    sptypes_from_meth_instance(mi) = Core.Compiler.sptypes_from_meth_instance(mi)
else
    sptypes_from_meth_instance(mi) = Core.Compiler.spvals_from_meth_instance(mi)
end

function find_callsites(CI, mi, slottypes; params=current_params(), kwargs...)
    sptypes = sptypes_from_meth_instance(mi)
    callsites = Callsite[]
    for (id, c) in enumerate(CI.code)
        if c isa Expr
            callsite = nothing
	    if c.head === :(=)
                c = c.args[2]
		(c isa Expr) || continue
	    end
            if c.head === :invoke
                rt = CI.ssavaluetypes[id]
                callsite = Callsite(id, c.args[1], rt)
            elseif c.head === :call
                rt = CI.ssavaluetypes[id]
                types = map(arg -> unwrap_type(Compiler.argextype(arg, CI, sptypes, slottypes)), c.args)

                # Filter out builtin functions and intrinsic function
                if types[1] <: Core.Builtin || types[1] <: Core.IntrinsicFunction
                    continue
                end

                # Filter out abstract signatures
                # otherwise generated functions get crumpy 
                if any(isabstracttype, types) || any(T->(T isa Union || T isa UnionAll), types)
                    continue
                end

                mi = first_method_instance(Tuple{types...})
                mi == nothing && continue
                callsite = Callsite(id, mi, rt)
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

current_params() = Core.Compiler.Params(ccall(:jl_get_world_counter, UInt, ()))

function first_method_instance(F, TT; params=current_params())
    sig = Tuple{typeof(F), TT.parameters...}
    first_method_instance(sig; params=params)
end

function first_method_instance(sig; params=current_params())
    methds = Base._methods_by_ftype(sig, 1, params.world)
    methds === false && return nothing
    x = methds[1]
    meth = x[3]
    if isdefined(meth, :generator) && !isdispatchtuple(Tuple{sig.parameters[2:end]...})
        return nothing
    end
    mi = Compiler.code_for_method(meth, sig, x[2], params.world)
end

