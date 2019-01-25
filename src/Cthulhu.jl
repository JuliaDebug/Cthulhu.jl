module Cthulhu

using TerminalMenus
using InteractiveUtils

export descend, @descend, descend_code_typed, descend_code_warntype, @descend_code_typed, @descend_code_warntype

struct Callsite
    id::Int # ssa-id
    f
    tt
    rt
end

mutable struct TextWidthLimiter
    io::IO
    width::Int
    limit::Int
end
TextWidthLimiter(io::IO, limit) = TextWidthLimiter(io, 0, limit)
has_space(limiter::TextWidthLimiter, width::Int) = limiter.width + width < limiter.limit - 1
has_space(limiter::TextWidthLimiter, s) = has_space(limiter, textwidth(string(s)))
function Base.print(io::TextWidthLimiter, s::String)
    io.width == io.limit && return 0
    width = textwidth(s::String)
    if has_space(io, width)
        print(io.io, s)
        io.width += width
        return
    else
        for c in graphemes(s)
            cwidth = textwidth(c)
            if has_space(io, cwidth)
                print(io, c)
                io.width += cwidth
            else
                break
            end
        end
        print(io, '…')
        io.width += 1
    end
end

function Base.show(io::IO, c::Callsite)
    limit = get(io, :limit, false)
    cols = limit ? displaysize(io)[2] : typemax(Int)
    limiter = TextWidthLimiter(io, cols)
    print(limiter, string("%", c.id, " = invoke "))
    if !has_space(limiter, c.f)
        print(limiter, '…')
        return
    end
    print(limiter, string(c.f))
    pstrings = map(string, c.tt.parameters)
    headstrings = map(x->string(x.name), c.tt.parameters)
    print(limiter, "(")
    # See if we have space to print all the parameters fully
    if has_space(limiter, sum(textwidth, pstrings) + 3*length(pstrings))
        print(limiter, join(map(T->string("::", T), pstrings), ","))
    # Alright, see if we at least have enough space for each head
    elseif has_space(limiter, sum(textwidth, headstrings) + 6*length(pstrings))
        print(limiter, join(map(T->string("::", T, "{…}"), headstrings), ","))
    # Fine, what about just indicating the number of arguments
    elseif has_space(limiter, 2*(length(c.tt.parameters)))
        print(limiter, join(map(T->"…", pstrings), ","))
    else
        print(limiter, "…")
    end
    print(limiter, ")")

    # If we have space for the return type, print it
    rts = string(c.rt)
    if has_space(limiter, textwidth(rts)+2)
        print(limiter, string("::", rts))
    end
end

function Callsite(id, mi, rt)
    f = getfield(mi.def.module, mi.def.name)
    tt = Tuple{mi.specTypes.parameters[2:end]...}
    return Callsite(id, f, tt, rt)
end

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

find_type(CI, TT, arg) = typeof(arg)
find_type(CI, TT, arg::Core.SSAValue) = CI.ssavaluetypes[arg.id]

function find_type(CI, TT, arg::Expr)
    @assert arg.head === :static_parameter
    T = typeof(arg.args[1])
end

function find_type(CI, TT, arg::Core.SlotNumber)
    slotid = arg.id - 1
    if slotid <= length(TT.parameters)
        return TT.parameters[slotid]
    end

    # find assignment
    root = nothing
    for c in CI.code
        if c isa Expr && c.head === :(=) && c.args[1] == arg
            root = c.args[2]
            break
        end
    end
    if root === nothing
        @warn "Could not find type of slot" arg
        return Union{}
    end
    find_type(CI, TT, root)
end

unwrap_type(T) = T
unwrap_type(T::Core.Compiler.Const) = typeof(T.val)

function find_callsites(CI, TT; kwargs...)
    callsites = Callsite[]
    for (id, c) in enumerate(CI.code)
        if c isa Expr
            callsite = nothing
            if c.head === :invoke
                rt = CI.ssavaluetypes[id]
                callsite = Callsite(id, c.args[1], rt)
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
                        dump(c)
                        continue
                    end
                else
                    @warn "Don't know how to handle call: " c
                    dump(c)
                    continue
                end

                args = c.args[2:end]
                types = map(arg -> unwrap_type(find_type(CI, TT, arg)), args)

                # Filter out builtin functions and intrinsic function
                if f isa Core.Builtin || f isa Core.IntrinsicFunction
                    continue
                end

                # Filter out abstract signatures
                # otherwise generated functions get crumpy 
                if any(isabstracttype, types) || any(T->(T isa Union || T isa UnionAll), types)
                    continue
                end

                callsite = Callsite(id, f, Tuple{types...}, rt)
            end

            if callsite !== nothing
                methods = code_typed(callsite.f, callsite.tt; kwargs...)
                if isempty(methods)
                    continue
                end
                push!(callsites, callsite)
            end
        end
    end
    return callsites
end

if VERSION >= v"1.1.0-DEV.215"
function dce!(code, TT)
    argtypes = Any[T for T in TT.parameters]
    ir = Core.Compiler.inflate_ir(code, Core.svec(), argtypes)
    compact = Core.Compiler.IncrementalCompact(ir, true)
    # Just run through the iterator without any processing
    Core.Compiler.foreach(x -> nothing, compact)
    ir = Core.Compiler.finish(compact)
    Core.Compiler.replace_code_newstyle!(code, ir, length(argtypes))
end
else
function dce!(code, TT)
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

function _descend(@nospecialize(F), @nospecialize(TT); iswarn::Bool, kwargs...)
    methods = code_typed(F, TT; kwargs...)
    if isempty(methods)
        println("$(string(Callsite(-1 ,F, TT, Any))) has no methods")
        return
    end
    CI, rt = first(methods)
    dce!(CI, TT)
    dce!(CI, TT)
    callsites = find_callsites(CI, TT; kwargs...)
    while true
        println()
        println("│ ─ $(string(Callsite(-1, F, TT, rt)))")
        iswarn ? code_warntype(F, TT) : display(CI=>rt)
        println()
        TerminalMenus.config(cursor = '•', scroll = :wrap)
        menu = RadioMenu(vcat(map(show_as_line, callsites), ["↩ "]))
        println("In `$F` select a call to descend into or ↩ to ascend. [q] to quit.")
        cid = request(menu)
        if cid == length(callsites) + 1
            break
        end
        if cid == -1
            throw(InterruptException())
        end
        callsite = callsites[cid]
        _descend(callsite.f, callsite.tt; iswarn=iswarn, kwargs...)
    end
end

end
