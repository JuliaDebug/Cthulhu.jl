module Cthulhu

using InteractiveUtils

using Core: MethodInstance
const Compiler = Core.Compiler

include("callsite.jl")
include("reflection.jl")
include("ui.jl")
include("codeview.jl")

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
descend_code_warntype(f, @nospecialize(tt); kwargs...) =
    _descend_with_error_handling(f, tt; iswarn=true, kwargs...)

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

##
# _descend is the main driver function.
# src/reflection.jl has the tools to discover methods
# src/ui.jl provides the user facing interface to which _descend responds
##
function _descend(mi::MethodInstance; iswarn::Bool, params=current_params(), optimize::Bool=true, kwargs...)
    display_CI = true
    debuginfo = true
    if :debuginfo in keys(kwargs)
        selected = kwargs[:debuginfo]
        # TODO: respect default
        debuginfo = selected == :source
    end

    while true
        (CI, rt, slottypes) = do_typeinf_slottypes(mi, optimize, params)
        preprocess_ci!(CI, mi, optimize)
        callsites = find_callsites(CI, mi, slottypes; params=params, kwargs...)

        debuginfo_key = debuginfo ? :source : :none
        if display_CI
            println()
            println("│ ─ $(string(Callsite(-1, MICallInfo(mi, rt))))")

            if iswarn
                cthulhu_warntype(stdout, CI, rt, debuginfo_key)
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

            if callsite.info isa MultiCallInfo
                sub_callsites = map(ci->Callsite(callsite.id, ci), callsite.info.callinfos)
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

            if callsite.info isa GeneratedCallInfo || callsite.info isa FailedCallInfo
                @error "Callsite %$(callsite.id) failed to be extracted" callsite
            end

            # recurse
            next_mi = get_mi(callsite)
            if next_mi === nothing
                continue
            end
            _descend(next_mi; params=params, optimize=optimize,
                     iswarn=iswarn, debuginfo=debuginfo_key, kwargs...)
        elseif toggle === :warn
            iswarn ⊻= true
        elseif toggle === :optimize
            optimize ⊻= true
        elseif toggle === :debuginfo
            debuginfo ⊻= true
        elseif toggle === :llvm
            cthulhu_llvm(stdout, mi, optimize, debuginfo, params, CONFIG)
            display_CI = false
        elseif toggle === :native
            cthulhu_native(stdout, mi, optimize, debuginfo, params, CONFIG)
            display_CI = false
        elseif toggle === :highlighter
            CONFIG.enable_highlighter ⊻= true
            if CONFIG.enable_highlighter
                @info "Using syntax highlighter $(CONFIG.highlighter)"
            else
                @info "Turned off syntax highlighter for LLVM and native code."
            end
            display_CI = false
        elseif toggle === :dump_params
            @info "Dumping inference cache"
            Core.show(map(((i, x),) -> (i, x.result, x.linfo), enumerate(params.cache)))
            Core.println()
            display_CI = false
        else
            error("Unknown option $toggle")
        end
    end
end

function _descend(@nospecialize(F), @nospecialize(TT); params=current_params(), kwargs...)
    mi = first_method_instance(F, TT; params=params)
    _descend(mi; params=params, kwargs...)
end

end
