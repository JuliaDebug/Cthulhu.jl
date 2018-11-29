module Cthulhu

using TerminalMenus
using InteractiveUtils

export descent

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
    descent(f, tt)

Given a function and a tuple-type allows to iterativly descent through
invoke statements.

# Usage:
```julia
function test()
    T = rand() > 0.5 ? Int64 : Float64
    sum(rand(T, 100))
end

descent(test, Tuple{})
```
"""
function descent(f, @nospecialize(tt))
    CI, rt = code_typed(f, tt)[1]
    callsites = collect(Callsite(c.args[1])
        for c in CI.code if c isa Expr &&
                            c.head === :invoke)
    display(CI=>rt)
    menu = RadioMenu(map(string, callsites))
    cid = request(menu)
    callsite = callsites[cid]
    descent(callsite.f, callsite.tt)
end

end
