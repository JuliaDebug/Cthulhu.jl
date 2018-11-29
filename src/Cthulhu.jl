module Cthulhu

using TerminalMenus
using InteractiveUtils

export explore_code_typed, @explore_code_typed

struct Callsite
    f
    tt
end

function Base.string(c::Callsite)
    "invoke($(c.f), $(c.tt))"
end

function Callsite(mi)
    f = getfield(mi.def.module, mi.def.name)
    tt = Tuple{mi.specTypes.parameters[2:end]...}
    return Callsite(f, tt)
end

"""
  @explore_code_typed

  Evaluates the arguments to the function or macro call, determines their
types, and calls `code_typed on the resulting expression.
"""
macro explore_code_typed(ex0...)
    InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :explore_code_typed, ex0)
end

"""
    explore_code_typed(f, tt)

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

explore_code_typed(foo, Tuple{})
```
"""
function explore_code_typed(f, @nospecialize(tt))
    try
        _explore_code_typed(f, tt)
    catch InterruptException
    end
    return nothing
end

function _explore_code_typed(f, @nospecialize(tt))
    CI, rt = code_typed(f, tt)[1]
    callsites = collect(Callsite(c.args[1])
        for c in CI.code if c isa Expr &&
                            c.head === :invoke)
    while true
        println()
        println("│ ─ $(string(Callsite(f, tt)))")
        display(CI=>rt)
        println()
        TerminalMenus.config(cursor = '•', scroll = :wrap)
        menu = RadioMenu(vcat(map(string, callsites), ["↩ "]))
        cid = request(menu)
        if cid == length(callsites) + 1
            break
        end
        if cid == -1
            throw(InterruptException())
        end
        callsite = callsites[cid]
        _explore_code_typed(callsite.f, callsite.tt)
    end
end

end
