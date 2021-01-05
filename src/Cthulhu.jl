module Cthulhu

using CodeTracking: definition, whereis
using InteractiveUtils
using UUIDs

using Core: MethodInstance
const Compiler = Core.Compiler

if isdefined(Base, :mapany)
    const mapany = Base.mapany
else
    mapany(f, itr) = map!(f, Vector{Any}(undef, length(itr)::Int), itr)  # convenient for Expr.args
end

Base.@kwdef mutable struct CthulhuConfig
    enable_highlighter::Bool = false
    highlighter::Cmd = `pygmentize -l`
    asm_syntax::Symbol = :att
    dead_code_elimination::Bool = true
    pretty_ast::Bool = false
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
- `pretty_ast::Bool`: Use a pretty printer for the ast dump. Defaults to false.
"""
const CONFIG = CthulhuConfig()

include("callsite.jl")
include("reflection.jl")
include("ui.jl")
include("codeview.jl")
include("backedges.jl")

export descend, @descend, descend_code_typed, descend_code_warntype, @descend_code_typed, @descend_code_warntype
export ascend

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
    descend_code_typed(Cthulhu.BOOKMARKS[i]; kwargs...)

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
    descend_code_warntype(Cthulhu.BOOKMARKS[i])

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

function _descend_with_error_handling(args...; kwargs...)
    @nospecialize
    try
        _descend(args...; kwargs...)
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

descend(mi::MethodInstance; kwargs...) = _descend(mi; iswarn=false, interruptexc=false, kwargs...)

##
# _descend is the main driver function.
# src/reflection.jl has the tools to discover methods
# src/ui.jl provides the user facing interface to which _descend responds
##
function _descend(mi::MethodInstance; iswarn::Bool, params=current_params(), optimize::Bool=true, interruptexc::Bool=true, verbose=true, kwargs...)
    debuginfo = true
    if :debuginfo in keys(kwargs)
        selected = kwargs[:debuginfo]
        # TODO: respect default
        debuginfo = selected === :source
    end

    display_CI = true
    while true
        debuginfo_key = debuginfo ? :source : :none
        (CI, rt, slottypes) = do_typeinf_slottypes(mi, optimize, params)
        preprocess_ci!(CI, mi, optimize, CONFIG)
        callsites = find_callsites(CI, mi, slottypes; params=params, kwargs...)

        display_CI && cthulu_typed(stdout, debuginfo_key, CI, rt, mi, iswarn, verbose)
        display_CI = true

        TerminalMenus.config(cursor = '•', scroll = :wrap)
        menu = CthulhuMenu(callsites, optimize)
        cid = request(menu)
        toggle = menu.toggle

        if toggle === nothing
            if cid == length(callsites) + 1
                break
            end
            if cid == -1
                interruptexc ? throw(InterruptException()) : break
            end
            callsite = callsites[cid]

            if callsite.info isa Union{MultiCallInfo,DeoptimizedCallInfo}
                callinfos = if callsite.info isa DeoptimizedCallInfo
                    [callsite.info.accurate, callsite.info.deoptimized]
                else
                    callsite.info.callinfos
                end
                sub_callsites = let callsite=callsite
                    map(ci->Callsite(callsite.id, ci, callsite.head), callinfos)
                end
                if isempty(sub_callsites)
                    @warn "Expected multiple callsites, but found none. Please fill an issue with a reproducing example" callsite.info
                    continue
                end
                menu = CthulhuMenu(sub_callsites, optimize; sub_menu=true)
                cid = request(menu)
                if cid == length(sub_callsites) + 1
                    continue
                end
                if cid == -1
                    interruptexc ? throw(InterruptException()) : break
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
                     iswarn=iswarn, debuginfo=debuginfo_key, interruptexc=interruptexc, verbose=verbose, kwargs...)

        elseif toggle === :warn
            iswarn ⊻= true
        elseif toggle === :verbose
            verbose ⊻= true
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
        elseif toggle === :bookmark
            push!(BOOKMARKS, Bookmark(mi, params))
            @info "The method is pushed at the end of `Cthulhu.BOOKMARKS`."
            display_CI = false
        elseif toggle === :revise
            # Call Revise.revise() without introducing a dependency on Revise
            id = Base.PkgId(UUID("295af30f-e4ad-537b-8983-00126c2a3abe"), "Revise")
            mod = get(Base.loaded_modules, id, nothing)
            if mod !== nothing
                revise = getfield(mod, :revise)::Function
                revise()
                mi = first_method_instance(mi.specTypes)
            end
        elseif toggle === :edit
            edit(whereis(mi.def)...)
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
    mi = choose_method_instance(F, TT; params=params)
    _descend(mi; params=params, kwargs...)
end

descend_code_typed(b::Bookmark; kw...) =
    _descend_with_error_handling(b.mi; iswarn = false, params = b.params, kw...)

descend_code_warntype(b::Bookmark; kw...) =
    _descend_with_error_handling(b.mi; iswarn = true, params = b.params, kw...)

if has_treemenu
    FoldingTrees.writeoption(buf::IO, data::Data, charsused::Int) = FoldingTrees.writeoption(buf, data.callstr, charsused)

    function ascend(mi; kwargs...)
        root = treelist(mi)
        menu = TreeMenu(root)
        choice = menu.current
        while choice !== nothing
            menu.chosen = false
            choice = TerminalMenus.request("Choose a call for analysis (q to quit):", menu; cursor=menu.currentidx)
            browsecodetyped = true
            if choice !== nothing
                node = menu.current
                mi = instance(node.data.nd)
                if !isroot(node)
                    # Help user find the sites calling the parent
                    miparent = instance(node.parent.data.nd)
                    params = current_params()
                    locs = []
                    for optimize in (true, false)
                        (CI, rt, slottypes) = do_typeinf_slottypes(mi, optimize, params)
                        preprocess_ci!(CI, mi, optimize, CONFIG)
                        callsites = find_callsites(CI, mi, slottypes; params=params)
                        callsites = filter(cs->is_callsite(cs, miparent), callsites)
                        append!(locs, CI.linetable[CI.codelocs[(cs->cs.id).(callsites)]])
                    end
                    if !isempty(locs)
                        ulocs = Dict{Tuple{Symbol,Symbol},Vector{Int}}()
                        for loc in locs
                            lines = get!(Vector{Int}, ulocs, (loc.method, loc.file))
                            line = loc.line
                            if line ∉ lines
                                push!(lines, line)
                            end
                        end
                        vlocs = collect(ulocs)
                        strlocs = [string('"', k[2], "\", ", k[1], ": lines ", v) for (k, v) in ulocs]
                        perm = sortperm(strlocs)
                        strlocs, vlocs = strlocs[perm], vlocs[perm]
                        push!(strlocs, "Browse typed code")
                        linemenu = TerminalMenus.RadioMenu(strlocs)
                        browsecodetyped = false
                        choice2 = 1
                        while choice2 != -1
                            choice2 = TerminalMenus.request("\nChoose caller of $miparent or proceed to typed code:", linemenu; cursor=choice2)
                            if 0 < choice2 < length(strlocs)
                                loc = vlocs[choice2]
                                edit(String(loc[1][2]), first(loc[2]))
                            elseif choice2 == length(strlocs)
                                browsecodetyped = true
                                break
                            end
                        end
                    end
                end
                # The main application of `ascend` is finding cases of non-inferrability, so the
                # warn highlighting is useful.
                browsecodetyped && _descend(mi; iswarn=true, optimize=false, interruptexc=false, kwargs...)
            end
        end
    end
else
    function ascend(mi; kwargs...)
        calls, mis = treelist(mi)
        menu = TerminalMenus.RadioMenu(calls)
        choice = 1
        while choice != -1
            choice = TerminalMenus.request("Choose a call for analysis (q to quit):", menu)
            if choice != -1
                # The main application of `ascend` is finding cases of non-inferrability, so the
                # warn highlighting is useful.
                _descend(instance(mis[choice]); iswarn=true, optimize=false, interruptexc=false, kwargs...)
            end
        end
    end
end

"""
    ascend(mi::MethodInstance; kwargs...)
    ascend(bt; kwargs...)

Follow a chain of calls (either through a backtrace `bt` or the backedges of a `MethodInstance` `mi`),
with the option to `descend` into intermediate calls. `kwargs` are passed to [`descend`](@ref).
"""
ascend

end
