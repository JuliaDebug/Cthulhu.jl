module Cthulhu

using TerminalMenus
using InteractiveUtils
using Requires

using Core: MethodInstance
const Compiler = Core.Compiler

include("callsite.jl")

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

"""
    substitute(callsite)

Walks the callsite of a function and substitute calls.
Useful for descending into GPU code.
"""
function substitute(callsite, args...; kwargs...)
    mi = callsite.mi
    if nameof(mi.def.module) == :CUDAnative &&
              mi.def.name   == :cufunction
        return transform(Val(:CuFunction), callsite, args...; kwargs...)
    else
        return callsite
    end
end

transform(::Val, callsite) = callsite
@init @require CUDAnative="be33ccc6-a3ff-5ff2-a52e-74243cff1e17" begin
    using .CUDAnative
    @eval function transform(::Val{:CuFunction}, callsite, callexpr, CI, oldMI, slottypes; params=nothing, kwargs....)
        spvals = Core.Compiler.spvals_from_meth_instance(mi)
        @show mi.specTypes
        @show spvals
        return callsite
    end
end

function find_callsites(CI, mi, slottypes; params=current_params(), kwargs...)
    spvals = Core.Compiler.spvals_from_meth_instance(mi)
    callsites = Callsite[]
    for (id, c) in enumerate(CI.code)
        if c isa Expr
            callsite = nothing
            if c.head === :invoke
                rt = CI.ssavaluetypes[id]
                callsite = Callsite(id, c.args[1], rt)
            elseif c.head === :call
                rt = CI.ssavaluetypes[id]
                types = map(arg -> unwrap_type(Compiler.argextype(arg, CI, spvals, slottypes)), c.args)

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
                callsite = substitute(callsite, callexpr, CI, mi, slottypes; params=params, kwargs...)
                push!(callsites, callsite)
            end
        end
    end
    return callsites
end

if VERSION >= v"1.1.0-DEV.215"
function dce!(ci, mi)
    argtypes = Core.Compiler.matching_cache_argtypes(mi, nothing)[1]
    ir = Compiler.inflate_ir(ci, Core.Compiler.spvals_from_meth_instance(mi),
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

function show_as_line(el)
    reduced_displaysize = displaysize(stdout) .- (0, 3)
    buf = IOBuffer()
    show(IOContext(buf, :limit=>true, :displasize=>reduced_displaysize), el)
    String(take!(buf))
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
function _descend(mi::MethodInstance; iswarn::Bool, params=current_params(), optimize::Bool=true, kwargs...)
    (CI, rt, slottypes) = do_typeinf_slottypes(mi, optimize, params)

    callsites = find_callsites(CI, mi, slottypes; params=params, kwargs...)
    callsites = substitute(callsites)
    while true
        println()
        println("│ ─ $(string(Callsite(-1, mi, rt)))")
        iswarn ? code_warntype(F, TT) : display(CI=>rt)
        println()
        TerminalMenus.config(cursor = '•', scroll = :wrap)
        menu = RadioMenu(vcat(map(show_as_line, callsites), ["↩ "]))
        println("In `$(mi.def.name)` select a call to descend into or ↩ to ascend. [q] to quit.")
        cid = request(menu)
        if cid == length(callsites) + 1
            break
        end
        if cid == -1
            throw(InterruptException())
        end
        callsite = callsites[cid]
        _descend(callsite.mi; iswarn=iswarn, kwargs...)
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
