module Cthulhu

using CodeTracking: definition, whereis
using InteractiveUtils
using UUIDs
using REPL: REPL, AbstractTerminal

using Core: MethodInstance
const Compiler = Core.Compiler
using Core.Compiler: MethodMatch, LimitedAccuracy, ignorelimited

const mapany = Base.mapany

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

module DInfo
    @enum DebugInfo none compact source
end
using .DInfo: DebugInfo

include("interpreter.jl")
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
    @interp

For debugging. Returns a CthulhuInterpreter from the appropriate entrypoint.
"""
macro interp(ex0...)
    InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :mkinterp, ex0)
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
const var"@descend" = var"@descend_code_typed"

"""
    descend_code_typed(f, tt=Tuple{}; kwargs...)
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

descend_code_typed(foo)
```
"""
descend_code_typed(f, @nospecialize(tt=Tuple{}); kwargs...) =
    _descend_with_error_handling(f, tt; iswarn=false, kwargs...)

"""
    descend_code_warntype(f, tt=Tuple{})
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

descend_code_warntype(foo)
```
"""
descend_code_warntype(f, @nospecialize(tt=Tuple{}); kwargs...) =
    _descend_with_error_handling(f, tt; iswarn=true, kwargs...)

function _descend_with_error_handling(args...; terminal=default_terminal(), kwargs...)
    @nospecialize
    try
        _descend(terminal, args...; kwargs...)
    catch x
        if x isa InterruptException
            return nothing
        else
            rethrow(x)
        end
    end
    return nothing
end

default_terminal() = REPL.LineEdit.terminal(Base.active_repl)

"""
    descend

Shortcut for [`descend_code_typed`](@ref).
"""
const descend = descend_code_typed

descend(interp::CthulhuInterpreter, mi::MethodInstance; kwargs...) = _descend(interp, mi; iswarn=false, interruptexc=false, kwargs...)

function codeinst_rt(code::CodeInstance)
    rettype = code.rettype
    if isdefined(code, :rettype_const)
        rettype_const = code.rettype_const
        if isa(rettype_const, Vector{Any}) && !(Vector{Any} <: rettype)
            return Core.PartialStruct(rettype, rettype_const)
        elseif rettype <: Core.OpaqueClosure && isa(rettype_const, Core.PartialOpaque)
            return rettype_const
        elseif isa(rettype_const, Core.InterConditional)
            return rettype_const
        else
            return Const(rettype_const)
        end
    else
        return rettype
    end
end

# `Base.@aggressive_constprop` here in order to make sure the constant propagation of `allow_no_codeinf`
Base.@aggressive_constprop function lookup(interp::CthulhuInterpreter, mi::MethodInstance, optimize::Bool; allow_no_codeinf::Bool=false)
    if !optimize
        codeinf = copy(interp.unopt[mi].src)
        rt = interp.unopt[mi].rt
        infos = interp.unopt[mi].stmt_infos
        slottypes = codeinf.slottypes
        if isnothing(slottypes)
            slottypes = Any[ Any for i = 1:length(codeinf.slotflags) ]
        end
    else
        codeinst = interp.opt[mi]
        rt = codeinst_rt(codeinst)
        codeinf0 = codeinst.inferred
        if codeinf0 !== nothing
            ir = Core.Compiler.copy(codeinf0.ir::IRCode)
            codeinf = ir
            infos = ir.stmts.info
            slottypes = ir.argtypes
        elseif allow_no_codeinf
            # This doesn't showed up as covered, but it is (see the CI test with `coverage=false`).
            # But with coverage on, the empty function body isn't empty due to :code_coverage_effect expressions.
            codeinf = nothing
            infos = []
            slottypes = Any[Base.unwrap_unionall(mi.specTypes).parameters...]
        else
            Core.eval(Main, quote
                interp = $interp
                mi = $mi
                optimize = $optimize
            end)
            error("couldn't find the source; inspect `Main.interp` and `Main.mi`")
        end
    end
    (codeinf, rt, infos, slottypes::Vector{Any})
end

##
# _descend is the main driver function.
# src/reflection.jl has the tools to discover methods
# src/ui.jl provides the user facing interface to which _descend responds
##
function _descend(term::AbstractTerminal, interp::CthulhuInterpreter, mi::MethodInstance;
    override::Union{Nothing,InferenceResult}=nothing, debuginfo::Union{Symbol,DebugInfo}=DInfo.compact, # default is compact debuginfo
    params=current_params(), optimize::Bool=true, interruptexc::Bool=true,
    iswarn::Bool=false, hide_type_stable::Union{Nothing,Bool}=nothing, verbose::Union{Nothing,Bool}=nothing, inline_cost::Bool=false)
    if isnothing(hide_type_stable)
        hide_type_stable = something(verbose, false)
    end
    isnothing(verbose) || Base.depwarn("The `verbose` keyword argument to `Cthulhu.descend` is deprecated. Use `hide_type_stable` instead.")
    if isa(debuginfo, Symbol)
        debuginfo = getfield(DInfo, debuginfo)::DebugInfo
    end

    is_cached(key) = haskey(optimize ? interp.opt : interp.unopt, key)

    menu_options = (cursor = '•', scroll_wrap = true)
    display_CI = true
    while true
        if override === nothing && optimize && interp.opt[mi].inferred === nothing
            # This was inferred to a pure constant - we have no code to show,
            # but make something up that looks plausible.
            if display_CI
                println(term.out_stream::IO)
                println(term.out_stream::IO, "│ ─ $(string(Callsite(-1, MICallInfo(mi, interp.opt[mi].rettype), :invoke)))")
                println(term.out_stream::IO, "│  return ", Const(interp.opt[mi].rettype_const))
                println(term.out_stream::IO)
            end
            callsites = Callsite[]
        else
            if override !== nothing
                codeinf, rt = optimize ? (override.src, override.result) : (interp.unopt[override].src, interp.unopt[override].rt)
                if optimize
                    if isa(codeinf, Compiler.OptimizationState)
                        codeinf = Compiler.copy(codeinf.ir::IRCode)
                        infos = codeinf.stmts.info
                        slottypes = codeinf.argtypes
                    else # source might be unavailable at this point, when a result is fully constant-folded etc.
                        (codeinf, rt, infos, slottypes) = lookup(interp, mi, optimize)
                    end
                else
                    codeinf = copy(codeinf)
                    infos = interp.unopt[override].stmt_infos
                    slottypes = codeinf.slottypes
                    if isnothing(slottypes)
                        slottypes = Any[ Any for i = 1:length(codeinf.slotflags) ]
                    end
                end
            else
                (codeinf, rt, infos, slottypes) = lookup(interp, mi, optimize)
            end
            ci = preprocess_ci!(codeinf, mi, optimize, CONFIG)
            callsites = find_callsites(interp, codeinf, infos, mi, slottypes, optimize; params)

            display_CI && cthulhu_typed(term.out_stream::IO, debuginfo,
                codeinf, rt, mi;
                iswarn, hide_type_stable, inline_cost)
            display_CI = true
        end

        menu = CthulhuMenu(callsites, optimize; menu_options...)
        cid = request(term, menu)
        toggle = menu.toggle

        if toggle === nothing
            if cid == length(callsites) + 1
                break
            end
            if cid == -1
                interruptexc ? throw(InterruptException()) : break
            end
            callsite = callsites[cid]

            info = callsite.info
            if info isa MultiCallInfo
                sub_callsites = map(ci->Callsite(callsite.id, ci, callsite.head), info.callinfos)
                if isempty(sub_callsites)
                    @warn "Expected multiple callsites, but found none. Please fill an issue with a reproducing example" info
                    continue
                end
                menu = CthulhuMenu(sub_callsites, optimize; sub_menu=true, menu_options...)
                cid = request(term, menu)
                if cid == length(sub_callsites) + 1
                    continue
                end
                if cid == -1
                    interruptexc ? throw(InterruptException()) : break
                end

                callsite = sub_callsites[cid]
                info = callsite.info
            end

            # forcibly enter and inspect the frame, although the native interpreter gave up
            if info isa UncachedCallInfo || info isa TaskCallInfo
                next_interp = CthulhuInterpreter()
                next_mi = get_mi(info)::MethodInstance
                do_typeinf!(next_interp, next_mi)
                _descend(term, next_interp, next_mi;
                         debuginfo,
                         params, optimize, interruptexc,
                         iswarn, hide_type_stable, inline_cost)
                continue
            end

            if info isa GeneratedCallInfo || callsite.info isa FailedCallInfo
                @error "Callsite %$(callsite.id) failed to be extracted" callsite
            end

            # recurse
            next_mi = get_mi(callsite)
            if next_mi === nothing
                continue
            end

            _descend(term, interp, next_mi;
                     override = isa(info, ConstPropCallInfo) ? info.result : nothing, debuginfo,
                     params,optimize, interruptexc,
                     iswarn, hide_type_stable, inline_cost)

        elseif toggle === :warn
            iswarn ⊻= true
        elseif toggle === :hide_type_stable
            hide_type_stable ⊻= true
        elseif toggle === :optimize
            optimize ⊻= true
            if !is_cached(mi)
                @warn "can't switch to post-optimization state, since this inference frame isn't cached"
                optimize ⊻= true
            end
        elseif toggle === :debuginfo
            debuginfo = DebugInfo((Int(debuginfo) + 1) % 3)
        elseif toggle === :inline_cost
            inline_cost ⊻= true
            if inline_cost && !optimize
                @warn "enable optimization to see the inlining costs"
            end
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
            push!(BOOKMARKS, Bookmark(mi, interp))
            @info "The method is pushed at the end of `Cthulhu.BOOKMARKS`."
            display_CI = false
        elseif toggle === :revise
            # Call Revise.revise() without introducing a dependency on Revise
            id = Base.PkgId(UUID("295af30f-e4ad-537b-8983-00126c2a3abe"), "Revise")
            mod = get(Base.loaded_modules, id, nothing)
            if mod !== nothing
                revise = getfield(mod, :revise)::Function
                revise()
                mi = get_specialization(mi.specTypes)
                do_typeinf!(interp, mi)
            end
        elseif toggle === :edit
            edit(whereis(mi.def::Method)...)
            display_CI = false
        else
            #Handle Standard alternative view, e.g. :native, :llvm
            view_cmd = get(CODEVIEWS, toggle, nothing)
            @assert !isnothing(view_cmd) "invalid option $toggle"
            println(term.out_stream)
            view_cmd(term.out_stream::IO, mi, optimize, debuginfo, params, CONFIG)
            display_CI = false
        end
        println(term.out_stream)
    end
end
_descend(interp::CthulhuInterpreter, mi::MethodInstance; kwargs...) =
    _descend(default_terminal(), interp::CthulhuInterpreter, mi::MethodInstance; kwargs...)

function do_typeinf!(interp::CthulhuInterpreter, mi::MethodInstance)
    result = InferenceResult(mi)
    # we may want to handle the case when `InferenceState(...)` returns `nothing`,
    # which indicates code generation of a `@generated` has been failed,
    # and show it in the UI in some way ?
    frame = InferenceState(result, true, interp)::InferenceState
    Core.Compiler.typeinf(interp, frame)
    return nothing
end

function get_specialization(@nospecialize(TT))
    match = Base._which(TT)
    mi = Core.Compiler.specialize_method(match)
    return mi
end

function get_specialization(@nospecialize(F), @nospecialize(TT))
    return get_specialization(Base.signature_type(F, TT))
end

function mkinterp(@nospecialize(args...))
    interp = CthulhuInterpreter()
    mi = get_specialization(args...)
    do_typeinf!(interp, mi)
    (interp, mi)
end

function _descend(@nospecialize(args...); kwargs...)
    (interp, mi) = mkinterp(args...)
    _descend(interp, mi; kwargs...)
end
function _descend(term::AbstractTerminal, @nospecialize(args...); kwargs...)
    (interp, mi) = mkinterp(args...)
    _descend(term, interp, mi; kwargs...)
end

descend_code_typed(b::Bookmark; kw...) =
    _descend_with_error_handling(b.interp, b.mi; iswarn=false, kw...)

descend_code_warntype(b::Bookmark; kw...) =
    _descend_with_error_handling(b.interp, b.mi; iswarn=true, kw...)

FoldingTrees.writeoption(buf::IO, data::Data, charsused::Int) = FoldingTrees.writeoption(buf, data.callstr, charsused)

function ascend(term, mi; kwargs...)
    root = treelist(mi)
    menu = TreeMenu(root)
    choice = menu.current
    while choice !== nothing
        menu.chosen = false
        choice = TerminalMenus.request(term, "Choose a call for analysis (q to quit):", menu; cursor=menu.currentidx)
        browsecodetyped = true
        if choice !== nothing
            node = menu.current
            mi = instance(node.data.nd)
            if !isroot(node)
                # Help user find the sites calling the parent
                miparent = instance(node.parent.data.nd)
                ulocs = find_caller_of(miparent, mi)
                if !isempty(ulocs)
                    strlocs = [string(" "^k[3] * '"', k[2], "\", ", k[1], ": lines ", v) for (k, v) in ulocs]
                    push!(strlocs, "Browse typed code")
                    linemenu = TerminalMenus.RadioMenu(strlocs)
                    browsecodetyped = false
                    choice2 = 1
                    while choice2 != -1
                        choice2 = TerminalMenus.request(term, "\nChoose caller of $miparent or proceed to typed code:", linemenu; cursor=choice2)
                        if 0 < choice2 < length(strlocs)
                            loc = ulocs[choice2]
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
            interp = CthulhuInterpreter()
            do_typeinf!(interp, mi)
            browsecodetyped && _descend(term, interp, mi; iswarn=true, optimize=false, interruptexc=false, kwargs...)
        end
    end
end
ascend(mi; kwargs...) = ascend(default_terminal(), mi; kwargs...)

"""
    ascend(mi::MethodInstance; kwargs...)
    ascend(bt; kwargs...)

Follow a chain of calls (either through a backtrace `bt` or the backedges of a `MethodInstance` `mi`),
with the option to `descend` into intermediate calls. `kwargs` are passed to [`descend`](@ref).
"""
ascend

end
