module Cthulhu

using TerminalMenus
using InteractiveUtils

export descend, @descend, descend_code_typed, @descend_code_typed

struct Callsite
    id::Int # ssa-id
    f
    tt
    rt
end

function Base.string(c::Callsite)
    io = IOBuffer()
    print(io, "%", c.id, " = invoke ", c.f, "(")
    TT = c.tt.parameters
    for T in TT
        print(io, "::", T, ",")
    end
    !isempty(TT) && seek(io, position(io)-1)
    print(io, ")", "::", c.rt)
    String(take!(io))
end

function Callsite(id, mi, rt)
    f = getfield(mi.def.module, mi.def.name)
    tt = Tuple{mi.specTypes.parameters[2:end]...}
    return Callsite(id, f, tt, rt)
end

"""
  @descend_code_typed

  Evaluates the arguments to the function or macro call, determines their
types, and calls `code_typed on the resulting expression.
"""
macro descend_code_typed(ex0...)
    InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :descend_code_typed, ex0)
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
function descend_code_typed(f, @nospecialize(tt); kwargs...)
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

"""
  descend

  Shortcut for [`descend_code_typed`](@ref).
"""
const descend = descend_code_typed

function _descend(@nospecialize(f), @nospecialize(tt); kwargs...)
    methods = code_typed(f, tt; kwargs...)
    if isempty(methods)
        println("$(string(Callsite(-1 ,f, tt, Any))) has no methods")
        return
    end
    CI, rt = first(methods)
    callsites = Callsite[]
    for (id, c) in enumerate(CI.code)
        if c isa Expr
            if c.head === :invoke
                rt = CI.ssavaluetypes[id]
                push!(callsites, Callsite(id, c.args[1], rt))
            elseif c.head === :call
                rt = CI.ssavaluetypes[id]
                if c.args[1] isa Function
                    f = c.args[1]
                elseif c.args[1] isa GlobalRef
                    mod = c.args[1].mod
                    name = c.args[1].name
                    f = getfield(mod, name)
                elseif c.args[1] isa Core.SSAValue
                    # probably somthing of form
                    # %1 = Base.Broadcast.materialize::Const(materialize, false)
                    # ...
                    # %9 = (%1)(%8)::Any
                    _T = CI.ssavaluetypes[c.args[1].id]
                    if _T isa Core.Compiler.Const && _T.val isa Function
                        f = _T.val
                    elseif _T isa Type
                        continue
                    else
                        @warn "Don't know how to handle call: " c
                        dump(c.args[1])
                        continue
                    end
                else
                    @warn "Don't know how to handle call: " c
                    dump(c.args[1])
                    continue
                end
                args = c.args[2:end]
                types = Any[]
                for arg in args
                    if arg isa Core.SSAValue
                        T = CI.ssavaluetypes[arg.id]
                    elseif arg isa Core.SlotNumber
                        T = tt.parameters[arg.id - 1]
                    elseif arg isa Expr # arrrgh
                        @assert arg.head === :static_parameter
                        T = typeof(args.args[1])
                    else
                        T = typeof(arg)
                    end
                    if T isa Core.Compiler.Const
                        T = typeof(T.val)
                    end
                    push!(types, T)
                end
                if f isa Core.Builtin || f isa Core.IntrinsicFunction
                    continue
                end
                push!(callsites, Callsite(id, f, Tuple{types...}, rt))
            end
        end
    end
    while true
        println()
        println("│ ─ $(string(Callsite(-1, f, tt, rt)))")
        display(CI=>rt)
        println()
        TerminalMenus.config(cursor = '•', scroll = :wrap)
        menu = RadioMenu(vcat(map(string, callsites), ["↩ "]))
        println("In `$f` select a call to descend into or ↩ to ascend. [q] to quit.")
        cid = request(menu)
        if cid == length(callsites) + 1
            break
        end
        if cid == -1
            throw(InterruptException())
        end
        callsite = callsites[cid]
        _descend(callsite.f, callsite.tt)
    end
end

end
