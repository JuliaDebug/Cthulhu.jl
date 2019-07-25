module Cthulhu

using InteractiveUtils
using TypedCodeUtils

import TypedCodeUtils: CallInfo, canreflect, reflect, current_params, Reflection,
                       MICallInfo, MultiCallInfo, FailedCallInfo, GeneratedCallInfo

using Core: MethodInstance
const Compiler = Core.Compiler

include("callsite.jl")
include("show_limited.jl")
include("ui.jl")
include("codeview.jl")


export descend, descend_code_typed, descend_code_warntype
export @descend, @descend_code_typed, @descend_code_warntype

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

"""
    descend

Shortcut for [`descend_code_typed`](@ref).
"""
const descend = descend_code_typed

# Cthulhu's inner loop
function _descend(ref::Reflection; iswarn::Bool, params=current_params(), optimize::Bool=true, kwargs...)
    display_CI = true
    debuginfo = true
    if :debuginfo in keys(kwargs)
        selected = kwargs[:debuginfo]
        # TODO: respect default
        debuginfo = selected == :source
    end

    ref = TypedCodeUtils.preprocess!(DefaultConsumer(), ref, optimize)
    callsites = find_callsites(ref)
    while true
        debuginfo_key = debuginfo ? :source : :none
        if display_CI
            println()
            # println("│ ─ $(string(Callsite(-1, MICallInfo(mi, rt))))")

            if iswarn
                cthulhu_warntype(stdout, ref.CI, ref.rt, debuginfo_key)
            elseif VERSION >= v"1.1.0-DEV.762"
                show(stdout, ref.CI, debuginfo = debuginfo_key)
            else
                display(ref.CI=>ref.rt)
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

            if isambiguous(callsite.info)
                sub_callsites = map(ci->Callsite(callsite.id, ci), collect(callsite.info))
                menu = CthulhuMenu(sub_callsites, sub_menu=true)
                cid = request(menu)
                if cid == length(sub_callsites) + 1
                    continue
                end
                if cid == -1
                    throw(InterruptException())
                end
                callsite = sub_callsites[cid]
            end

            # recurse
            if canreflect(callsite)
                next_ref = reflect(callsite)
                _descend(next_ref; params=params, optimize=optimize,
                         iswarn=iswarn, debuginfo=debuginfo_key, kwargs...)
            else
                @warn "Could not descend into %$(callsite.id)" callsite
                 continue
            end
        elseif toggle === :warn
            iswarn ⊻= true
        elseif toggle === :optimize
            optimize ⊻= true
        elseif toggle === :debuginfo
            debuginfo ⊻= true
        elseif toggle === :llvm
            cthulhu_llvm(stdout, ref.mi, optimize, debuginfo, params)
            display_CI = false
        elseif toggle === :native
            cthulhu_native(stdout, ref.mi, optimize, debuginfo, params)
            display_CI = false
        elseif toggle === :dump_params
            @info "Dumping inference cache"
            Core.show(map(((i, x),) -> (i, x.result, x.linfo), enumerate(params.cache)))
            display_CI = false
        else
            error("Unknown option $toggle")
        end
    end
    end
end

function _descend(@nospecialize(F), @nospecialize(TT); kwargs...)
    reflections = reflect(F, TT; kwargs...)
    if length(reflections) == 0
        @info "Can't reflect upon" F, TT
    end
    ref = if length(reflections) == 1
        first(reflections)
    else
        callsites = map(r->Callsite(-1, MICallInfo(r.mi, Any)), _ref)
        menu = CthulhuMenu(callsites, sub_menu=true)
        cid = request(menu)
        if cid == length(sub_callsites) + 1
            return
        end
        if cid == -1
            throw(InterruptException())
        end
        reflections[cid]
    end
    _descend(ref; kwargs...)
end
