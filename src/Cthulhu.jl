module Cthulhu

using InteractiveUtils

using Core: MethodInstance
const Compiler = Core.Compiler

include("callsite.jl")
include("ui.jl")
include("reflection.jl")

export descend, @descend, descend_code_typed, descend_code_warntype, @descend_code_typed, @descend_code_warntype

"""
  @descend_code_typed

  Evaluates the arguments to the function or macro call, determines their
types, and calls `code_typed` on the resulting expression.
"""
macro descend_code_typed(ex0...)
    InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :descend_code_typed, ex0)
end

"""
  @descend_code_warntype

  Evaluates the arguments to the function or macro call, determines their
types, and calls `code_warntype` on the resulting expression.
"""
macro descend_code_warntype(ex0...)
    InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :descend_code_warntype, ex0)
end

"""
  @descend

  Shortcut for [`@descend_code_typed`](@ref).
"""
macro descend(ex0...)
    esc(:(@descend_code_typed($(ex0...))))
end

"""
    descend_code_typed(f, tt; kwargs...)

Given a function and a tuple-type, interactively explore the output of
`code_typed` by descending into `invoke` statements. Type enter to select an
`invoke` to descend into, select ↩  to ascend, and press q or control-c to
quit.

# Usage:
```julia
function foo()
    T = rand() > 0.5 ? Int64 : Float64
    sum(rand(T, 100))
end

descend_code_typed(foo, Tuple{})
```
"""
descend_code_typed(f, @nospecialize(tt); kwargs...) =
    _descend_with_error_handling(f, tt; iswarn=false, kwargs...)

"""
    descend_code_warntype(f, tt)

Given a function and a tuple-type, interactively explore the output of
`code_warntype` by descending into `invoke` statements. Type enter to select an
`invoke` to descend into, select ↩  to ascend, and press q or control-c to
quit.

# Usage:
```julia
function foo()
    T = rand() > 0.5 ? Int64 : Float64
    sum(rand(T, 100))
end

descend_code_warntype(foo, Tuple{})
```
"""
descend_code_warntype(f, @nospecialize(tt)) =
    _descend_with_error_handling(f, tt; iswarn=true)

function _descend_with_error_handling(f, @nospecialize(tt); kwargs...)
    try
        _descend(f, tt; kwargs...)
    catch x
        if x isa InterruptException
            return nothing
        else
            rethrow(x)
        end
    end
    return nothing
end

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

"""
  descend

  Shortcut for [`descend_code_typed`](@ref).
"""
const descend = descend_code_typed

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
function _descend(mi::MethodInstance; iswarn::Bool, params=current_params(), optimize::Bool=true, kwargs...)
    display_CI = true
    debuginfo = true
    if :debuginfo in keys(kwargs)
        selected = ans[:debuginfo]
        # TODO: respect default
        debuginfo = selected == :source 
    end

    while true
        (CI, rt, slottypes) = do_typeinf_slottypes(mi, optimize, params)
        preprocess_ci!(CI, mi, optimize)
        callsites = find_callsites(CI, mi, slottypes; params=params, kwargs...)

        if display_CI
            println()
            println("│ ─ $(string(Callsite(-1, mi, rt)))")
    
            debuginfo_key = debuginfo ? :source : :none
            if iswarn
                cthulhu_warntype(CI, rt, debuginfo_key)
            elseif VERSION >= v"1.1.0-DEV.762"
                show(stdout, CI, debuginfo = debuginfo_key)
            else
                display(CI=>rt)
            end
            println()
        end
        display_CI = true

        TerminalMenus.config(cursor = '•', scroll = :wrap)
        menu = CthulhuMenu(callsites)
        cid = request(menu)
        toggle = menu.toggle

        if toggle === nothing
            if cid == length(callsites) + 1
                break
            end
            if cid == -1
                throw(InterruptException())
            end
            callsite = callsites[cid]
            # recurse
            _descend(callsite.mi; iswarn=iswarn, kwargs...)

        elseif toggle === :warn
            iswarn ⊻= true
        elseif toggle === :optimize
            optimize ⊻= true
        elseif toggle === :debuginfo
            debuginfo ⊻= true
        elseif toggle === :llvm
            cthulhu_llvm(mi)
            display_CI = false
        elseif toggle === :native
            cthulhu_native(mi)
            display_CI = false
        else
            error("Unknown option $toggle")
        end
    end
end

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

function _descend(@nospecialize(F), @nospecialize(TT); params=current_params(), kwargs...)
    mi = first_method_instance(F, TT; params=params)
    _descend(mi; params=params, kwargs...)
end

end
