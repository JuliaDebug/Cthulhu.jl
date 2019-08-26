module Cthulhu

using CodeTracking: definition
using InteractiveUtils

using Core: MethodInstance
const Compiler = Core.Compiler

Base.@kwdef mutable struct CthulhuConfig
    enable_highlighter::Bool = false
    highlighter::Cmd = `pygmentize -l`
    asm_syntax::Symbol = :att
    dead_code_elimination::Bool = true
end

"""
    Cthulhu.CONFIG

# Options
- `enable_highlighter::Bool`: Use command line `highlighter` to syntax highlight
  LLVM and native code.  Set to `true` if `highlighter` exists at the import
  time.
- `highlighter::Cmd`: A command line program that receives "llvm" or "asm" as
  an argument and the code as stdin.  Defaults to `$(CthulhuConfig().highlighter)`.
- `asm_syntax::Symbol`: Set the syntax of assembly code being used.
  Defaults to `$(CthulhuConfig().asm_syntax)`.
- `dead_code_elimination::Bool`: Enable dead-code elimination for high-level Julia IR.
  Defaults to `true`. DCE is known to be buggy and you may want to disable it if you
  encounter errors. Please report such bugs with a MWE to Julia or Cthulhu. 
"""
const CONFIG = CthulhuConfig()

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
    debuginfo = true
    if :debuginfo in keys(kwargs)
        selected = kwargs[:debuginfo]
        # TODO: respect default
        debuginfo = selected == :source
    end
    debuginfo_key = debuginfo ? :source : :none

    display_CI = true
    while true
        (CI, rt, slottypes) = do_typeinf_slottypes(mi, optimize, params)
        preprocess_ci!(CI, mi, optimize, CONFIG)
        callsites = find_callsites(CI, mi, slottypes; params=params, kwargs...)

        display_CI && cthulu_typed(stdout, debuginfo_key, CI, rt, mi, iswarn)
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
            #Handle Standard alternative view, e.g. :native, :llvm
            view_cmd = get(codeviews, toggle, nothing)
            if view_cmd !== nothing
                view_cmd(stdout, mi, optimize, debuginfo, params, CONFIG)
                display_CI = false
            else
                error("Unknown option $toggle")
            end
        end
    end
end

function _descend(@nospecialize(F), @nospecialize(TT); params=current_params(), kwargs...)
    mi = first_method_instance(F, TT; params=params)
    _descend(mi; params=params, kwargs...)
end

end
