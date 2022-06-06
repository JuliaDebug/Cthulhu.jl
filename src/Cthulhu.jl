module Cthulhu

Base.Experimental.@compiler_options compile=min optimize=1

using CodeTracking: definition, whereis
using InteractiveUtils
using UUIDs
using REPL: REPL, AbstractTerminal

using Core: MethodInstance
const Compiler = Core.Compiler
import Core.Compiler: MethodMatch, LimitedAccuracy, ignorelimited, specialize_method
import Base: unwrapva, isvarargtype, unwrap_unionall, rewrap_unionall
const mapany = Base.mapany

const ArgTypes = Vector{Any}

@static if !isdefined(Core.Compiler, :Effects)
    const Effects = Nothing
    const EFFECTS_TOTAL = nothing
    const EFFECTS_ENABLED = false
else
    const Effects = Core.Compiler.Effects
    const EFFECTS_TOTAL = Core.Compiler.EFFECTS_TOTAL
    const EFFECTS_ENABLED = true
end

import Base: @constprop

Base.@kwdef mutable struct CthulhuConfig
    enable_highlighter::Bool = false
    highlighter::Cmd = `pygmentize -l`
    asm_syntax::Symbol = :att
    dead_code_elimination::Bool = true
    pretty_ast::Bool = false
    debuginfo::Symbol = :compact
    optimize::Bool = true
    iswarn::Bool = false
    remarks::Bool = false
    with_effects::Bool = false
    inline_cost::Bool = false
    type_annotations::Bool = true
end

"""
    Cthulhu.CONFIG

# Options
- `enable_highlighter::Bool`: Use command line `highlighter` to syntax highlight
  Julia, LLVM and native code.
- `highlighter::Cmd`: A command line program that receives "julia" as an argument and julia
   code as stdin. Defaults to `$(CthulhuConfig().highlighter)`.
- `asm_syntax::Symbol`: Set the syntax of assembly code being used.
  Defaults to `$(CthulhuConfig().asm_syntax)`.
- `dead_code_elimination::Bool`: Enable dead-code elimination for high-level Julia IR.
  Defaults to `true`. DCE is known to be buggy and you may want to disable it if you
  encounter errors. Please report such bugs with a MWE to Julia or Cthulhu.
- `pretty_ast::Bool`: Use a pretty printer for the ast dump. Defaults to `false`.
- `debuginfo::Symbol`: Initial state of "debuginfo" toggle. Defaults to `:compact`.
  Options:. `:none`, `:compact`, `:source`
- `optimize::Bool`: Initial state of "optimize" toggle. Defaults to `true`.
- `iswarn::Bool`: Initial state of "warn" toggle. Defaults to `false`.
- `remarks::Bool` Initial state of "remarks" toggle. Defaults to `false`.
- `with_effects::Bool` Intial state of "effects" toggle. Defaults to `false`.
- `inline_cost::Bool` Initial state of "inlining costs" toggle. Defaults to `false`.
- `type_annotations::Bool` Initial state of "type annnotations" toggle. Defaults to `true`.
"""
const CONFIG = CthulhuConfig()

using Preferences
include("preferences.jl")
read_config!(CONFIG)

module DInfo
    @enum DebugInfo none compact source
end
using .DInfo: DebugInfo
const AnyDebugInfo = Union{DebugInfo,Symbol}

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
    descend_code_typed(f, argtypes=Tuple{...}; kwargs...)
    descend_code_typed(tt::Type{<:Tuple}; kwargs...)
    descend_code_typed(Cthulhu.BOOKMARKS[i]; kwargs...)
    descend_code_typed(mi::MethodInstance; kwargs...)

Given a function and a tuple-type, interactively explore the output of
`code_typed` by descending into `invoke` statements. Type enter to select an
`invoke` to descend into, select ↩  to ascend, and press q or control-c to
quit.

# Usage:
```julia
julia> descend_code_typed(sin, (Int,))
[...]

julia> descend_code_typed(sin, Tuple{Int})
[...]

julia> descend_code_typed(Tuple{typeof(sin), Int})
[...]

julia> descend_code_typed() do
           T = rand() > 0.5 ? Int64 : Float64
           sin(rand(T)
       end
[...]
```
"""
descend_code_typed(@nospecialize(args...); kwargs...) =
    _descend_with_error_handling(args...; iswarn=false, kwargs...)

"""
    descend_code_warntype(f, argtypes=Tuple{...}; kwargs...)
    descend_code_warntype(tt::Type{<:Tuple}; kwargs...)
    descend_code_warntype(Cthulhu.BOOKMARKS[i])
    descend_code_warntype(mi::MethodInstance; kwargs...)

Given a function and a tuple-type, interactively explore the output of
`code_warntype` by descending into `invoke` statements. Type enter to select an
`invoke` to descend into, select ↩  to ascend, and press q or control-c to
quit.

# Usage:
```julia
julia> descend_code_warntype(sin, (Int,))
[...]

julia> descend_code_warntype(sin, Tuple{Int})
[...]

julia> descend_code_warntype(Tuple{typeof(sin), Int})
[...]

julia> descend_code_warntype() do
           T = rand() > 0.5 ? Int64 : Float64
           sin(rand(T)
       end
[...]
```
"""
descend_code_warntype(@nospecialize(args...); kwargs...) =
    _descend_with_error_handling(args...; iswarn=true, kwargs...)

@static if isdefined(Base, :default_tt)
import Base: default_tt
else
function default_tt(@nospecialize(f))
    ms = methods(f)
    if length(ms) == 1
        return Base.tuple_type_tail(only(ms).sig)
    else
        return Tuple
    end
end
end

function _descend_with_error_handling(@nospecialize(f), @nospecialize(argtypes = default_tt(f)); kwargs...)
    ft = Core.Typeof(f)
    if isa(argtypes, Type)
        u = unwrap_unionall(argtypes)
        tt = rewrap_unionall(Tuple{ft, u.parameters...}, argtypes)
    else
        tt = Tuple{ft, argtypes...}
    end
    __descend_with_error_handling(tt; kwargs...)
end
_descend_with_error_handling(@nospecialize(tt::Type{<:Tuple}); kwargs...) =
    __descend_with_error_handling(tt; kwargs...)
_descend_with_error_handling(interp::AbstractInterpreter, mi::MethodInstance; kwargs...) =
    __descend_with_error_handling(interp, mi; kwargs...)
function __descend_with_error_handling(args...; terminal=default_terminal(), kwargs...)
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

function descend_code_typed(mi::MethodInstance; terminal=default_terminal(), kwargs...)
    interp = Cthulhu.CthulhuInterpreter()
    Cthulhu.do_typeinf!(interp, mi)
    _descend(terminal, interp, mi; iswarn=false, interruptexc=false, kwargs...)
end
function descend_code_warntype(mi::MethodInstance; terminal=default_terminal(), kwargs...)
    interp = Cthulhu.CthulhuInterpreter()
    Cthulhu.do_typeinf!(interp, mi)
    _descend(terminal, interp, mi; iswarn=true, interruptexc=false, kwargs...)
end

descend(interp::CthulhuInterpreter, mi::MethodInstance; kwargs...) = _descend(interp, mi; iswarn=false, interruptexc=false, kwargs...)

function codeinst_rt(code::CodeInstance)
    rettype = code.rettype
    if isdefined(code, :rettype_const)
        rettype_const = code.rettype_const
        if isa(rettype_const, Vector{Any}) && !(Vector{Any} <: rettype)
            return Core.PartialStruct(rettype, rettype_const)
        elseif isa(rettype_const, Core.PartialOpaque) && rettype <: Core.OpaqueClosure
            return rettype_const
        elseif isa(rettype_const, Core.Compiler.InterConditional) && !(Core.Compiler.InterConditional <: rettype)
            return rettype_const
        else
            return Const(rettype_const)
        end
    else
        return rettype
    end
end

@static if EFFECTS_ENABLED
    get_effects(codeinst::CodeInstance) = Core.Compiler.decode_effects(codeinst.ipo_purity_bits)
    get_effects(codeinst::CodeInfo) = Core.Compiler.decode_effects(codeinst.purity)
    get_effects(src::InferredSource) = src.effects
    get_effects(unopt::Dict{Union{MethodInstance, InferenceResult}, InferredSource}, mi::MethodInstance) =
        haskey(unopt, mi) ? get_effects(unopt[mi]) : Effects()
    get_effects(result::InferenceResult) = result.ipo_effects
    @static if VERSION ≥ v"1.9.0-DEV.409"
        get_effects(result::Compiler.ConstPropResult) = get_effects(result.result)
        get_effects(result::Compiler.ConcreteResult) = result.effects
    else
        get_effects(result::Compiler.ConstResult) = result.effects
    end
else
    get_effects(_...) = nothing
end

# `@constprop :aggressive` here in order to make sure the constant propagation of `allow_no_src`
@constprop :aggressive function lookup(interp::CthulhuInterpreter, mi::MethodInstance, optimize::Bool; allow_no_src::Bool=false)
    if !optimize
        codeinf = src = copy(interp.unopt[mi].src)
        rt = interp.unopt[mi].rt
        infos = interp.unopt[mi].stmt_info
        effects = get_effects(interp.unopt[mi])
        slottypes = src.slottypes
        if isnothing(slottypes)
            slottypes = Any[ Any for i = 1:length(src.slotflags) ]
        end
    else
        codeinst = interp.opt[mi]
        rt = codeinst_rt(codeinst)
        opt = codeinst.inferred
        if opt !== nothing
            opt = opt::OptimizedSource
            src = Core.Compiler.copy(opt.ir)
            codeinf = opt.src
            infos = src.stmts.info
            slottypes = src.argtypes
        elseif allow_no_src
            # This doesn't showed up as covered, but it is (see the CI test with `coverage=false`).
            # But with coverage on, the empty function body isn't empty due to :code_coverage_effect expressions.
            codeinf = src = nothing
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
        effects = get_effects(codeinst)
    end
    # NOTE return `codeinf::CodeInfo` in any case since it can provide additional information on slot names
    (; src, rt, infos, slottypes, codeinf, effects)
end

##
# _descend is the main driver function.
# src/reflection.jl has the tools to discover methods
# src/ui.jl provides the user facing interface to which _descend responds
##
function _descend(term::AbstractTerminal, interp::CthulhuInterpreter, mi::MethodInstance;
    override::Union{Nothing,InferenceResult}=nothing,
    debuginfo::Union{Symbol,DebugInfo}=CONFIG.debuginfo,    # default is compact debuginfo
    optimize::Bool=CONFIG.optimize,                         # default is true
    interruptexc::Bool=true,
    iswarn::Bool=CONFIG.iswarn,                             # default is false
    hide_type_stable::Union{Nothing,Bool}=nothing, verbose::Union{Nothing,Bool}=nothing,
    remarks::Bool=CONFIG.remarks&!CONFIG.optimize,          # default is false
    with_effects::Bool=CONFIG.with_effects,                 # default is false
    inline_cost::Bool=CONFIG.inline_cost&CONFIG.optimize,   # default is false
    type_annotations::Bool=CONFIG.type_annotations          # default is true
    )

    if isnothing(hide_type_stable)
        hide_type_stable = something(verbose, false)
    end
    isnothing(verbose) || Base.depwarn("The `verbose` keyword argument to `Cthulhu.descend` is deprecated. Use `hide_type_stable` instead.")
    if isa(debuginfo, Symbol)
        debuginfo = getfield(DInfo, debuginfo)::DebugInfo
    end

    iscached(key, opt::Bool) = haskey(opt ? interp.opt : interp.unopt, key)

    menu_options = (cursor = '•', scroll_wrap = true)
    display_CI = true
    view_cmd = cthulhu_typed
    iostream = term.out_stream::IO
    while true
        if override === nothing && optimize && interp.opt[mi].inferred === nothing
            # This was inferred to a pure constant - we have no code to show,
            # but make something up that looks plausible.
            if display_CI
                println(iostream)
                println(iostream, "│ ─ $(string(Callsite(-1, MICallInfo(mi, interp.opt[mi].rettype, get_effects(interp.opt[mi])), :invoke)))")
                println(iostream, "│  return ", Const(interp.opt[mi].rettype_const))
                println(iostream)
            end
            callsites = Callsite[]
        else
            if override !== nothing
                if optimize
                    opt = override.src
                    rt = override.result
                    if isa(opt, Compiler.OptimizationState)
                        src = Compiler.copy(src.ir::IRCode)
                        codeinf = src.src
                        infos = opt.stmts.info
                        slottypes = opt.argtypes
                        effects = get_effects(codeinf)
                    elseif isa(opt, OptimizedSource)
                        # `(override::InferenceResult).src` might has been transformed to OptimizedSource already,
                        # e.g. when we switch from constant-prop' unoptimized source
                        src = Core.Compiler.copy(opt.ir)
                        codeinf = opt.src
                        infos = src.stmts.info
                        slottypes = src.argtypes
                        effects = opt.effects
                    else
                        # the source might be unavailable at this point,
                        # when a result is fully constant-folded etc.
                        (; src, rt, infos, slottypes, codeinf, effects) = lookup(interp, mi, optimize)
                    end
                else
                    unopt = get(interp.unopt, override, missing)
                    if unopt === missing
                        unopt = interp.unopt[override.linfo]
                    end
                    codeinf = src = copy(unopt.src)
                    rt = unopt.rt
                    infos = unopt.stmt_info
                    effects = get_effects(unopt)
                    slottypes = src.slottypes
                    if isnothing(slottypes)
                        slottypes = Any[ Any for i = 1:length(src.slotflags) ]
                    end
                end
            else
                (; src, rt, infos, slottypes, codeinf, effects) = lookup(interp, mi, optimize)
            end
            src = preprocess_ci!(src, mi, optimize, CONFIG)
            if optimize # optimization might have deleted some statements
                infos = src.stmts.info
            else
                @assert length(src.code) == length(infos)
            end
            callsites = find_callsites(interp, src, infos, mi, slottypes, optimize)

            if display_CI
                _remarks = remarks ? get(interp.remarks, mi, nothing) : nothing
                if with_effects
                    printstyled(IOContext(iostream, :limit=>true), '[', effects, ']', mi.def, '\n'; bold=true)
                else
                    printstyled(IOContext(iostream, :limit=>true), mi.def, '\n'; bold=true)
                end
                if debuginfo == DInfo.compact
                    str = let debuginfo=debuginfo, src=src, codeinf=codeinf, rt=rt, mi=mi,
                              iswarn=iswarn, hide_type_stable=hide_type_stable,
                              remarks=_remarks, inline_cost=inline_cost, type_annotations=type_annotations
                        ioctx = IOContext(iostream, :color=>true, :displaysize=>displaysize(iostream)) # displaysize doesn't propagate otherwise
                        stringify(ioctx) do io
                            lambda_io = IOContext(io, :SOURCE_SLOTNAMES => Base.sourceinfo_slotnames(codeinf))
                            cthulhu_typed(lambda_io, debuginfo, src, rt, mi;
                                          iswarn, hide_type_stable,
                                          remarks, inline_cost, type_annotations,
                                          interp)
                        end
                    end
                    # eliminate trailing indentation (see first item in bullet list in PR #189)
                    rmatch = findfirst(r"\u001B\[90m\u001B\[(\d+)G( *)\u001B\[1G\u001B\[39m\u001B\[90m( *)\u001B\[39m$", str)
                    if rmatch !== nothing
                        str = str[begin:prevind(str, first(rmatch))]
                    end
                    print(iostream, str)
                else
                    lambda_io = IOContext(iostream, :SOURCE_SLOTNAMES => Base.sourceinfo_slotnames(codeinf))
                    cthulhu_typed(lambda_io, debuginfo, src, rt, mi;
                                  iswarn, hide_type_stable,
                                  remarks=_remarks, inline_cost, type_annotations,
                                  interp)
                end
                view_cmd = cthulhu_typed
            end
            display_CI = true
        end

        menu = CthulhuMenu(callsites, with_effects, optimize, iswarn&get(iostream, :color, false)::Bool; menu_options...)
        usg = usage(view_cmd, optimize, iswarn, hide_type_stable, debuginfo, remarks, with_effects, inline_cost, type_annotations, CONFIG.enable_highlighter)
        cid = request(term, usg, menu)
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
                sub_callsites = let callsite=callsite
                    map(ci->Callsite(callsite.id, ci, callsite.head), info.callinfos)
                end
                if isempty(sub_callsites)
                    @warn "Expected multiple callsites, but found none. Please fill an issue with a reproducing example" info
                    continue
                end
                menu = CthulhuMenu(sub_callsites, with_effects, optimize, false; sub_menu=true, menu_options...)
                cid = request(term, "", menu)
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
                # XXX: this may use a different native interpreter
                next_interp = CthulhuInterpreter()
                next_mi = get_mi(info)::MethodInstance
                do_typeinf!(next_interp, next_mi)
                _descend(term, next_interp, next_mi;
                         debuginfo,
                         optimize, interruptexc,
                         iswarn, hide_type_stable,
                         remarks, with_effects, inline_cost, type_annotations)
                continue
            end

            if info isa GeneratedCallInfo || callsite.info isa FailedCallInfo
                @error "Callsite %$(callsite.id) failed to be extracted" callsite
            end

            # recurse
            next_mi = get_mi(callsite)::Union{MethodInstance,Nothing}
            if next_mi === nothing
                continue
            end

            _descend(term, interp, next_mi;
                     override = isa(info, ConstPropCallInfo) ? info.result : nothing, debuginfo,
                     optimize, interruptexc,
                     iswarn, hide_type_stable,
                     remarks, with_effects, inline_cost, type_annotations)

        elseif toggle === :warn
            iswarn ⊻= true
        elseif toggle === :with_effects
            with_effects ⊻= true
        elseif toggle === :hide_type_stable
            hide_type_stable ⊻= true
        elseif toggle === :optimize
            optimize ⊻= true
            if !iscached(mi, optimize)
                @warn "can't switch to post-optimization state, since this inference frame isn't cached"
                optimize ⊻= true
            end
            if remarks && optimize
                @warn "disable optimization to see the inference remarks"
            end
        elseif toggle === :debuginfo
            debuginfo = DebugInfo((Int(debuginfo) + 1) % 3)
        elseif toggle === :remarks
            remarks ⊻= true
            if remarks && optimize
                @warn "disable optimization to see the inference remarks"
            end
        elseif toggle === :inline_cost
            inline_cost ⊻= true
            if inline_cost && !optimize
                @warn "enable optimization to see the inlining costs"
            end
        elseif toggle === :type_annotations
            type_annotations ⊻= true
        elseif toggle === :highlighter
            CONFIG.enable_highlighter ⊻= true
            if CONFIG.enable_highlighter
                @info "Using syntax highlighter $(CONFIG.highlighter)"
            else
                @info "Turned off syntax highlighter for Julia, LLVM and native code."
            end
            display_CI = false
        elseif toggle === :dump_params
            @info "Dumping inference cache"
            Core.show(mapany(((i, x),) -> (i, x.result, x.linfo), enumerate(get_inference_cache(interp))))
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
                mi = get_specialization(mi.specTypes)::MethodInstance
                do_typeinf!(interp, mi)
            end
        elseif toggle === :edit
            edit(whereis(mi.def::Method)...)
            display_CI = false
        else
            #Handle Standard alternative view, e.g. :native, :llvm
            if toggle === :typed
                view_cmd = cthulhu_typed
                display_CI = true
            else
                view_cmd = get(CODEVIEWS, toggle, nothing)
                @assert !isnothing(view_cmd) "invalid option $toggle"
                println(iostream)
                view_cmd(iostream, mi, optimize, debuginfo, interp, CONFIG)
                display_CI = false
            end
        end
        println(iostream)
    end
end
_descend(interp::CthulhuInterpreter, mi::MethodInstance; kwargs...) =
    _descend(default_terminal(), interp::CthulhuInterpreter, mi::MethodInstance; kwargs...)

function do_typeinf!(interp::CthulhuInterpreter, mi::MethodInstance)
    result = InferenceResult(mi)
    # we may want to handle the case when `InferenceState(...)` returns `nothing`,
    # which indicates code generation of a `@generated` has been failed,
    # and show it in the UI in some way ?
    # branch on https://github.com/JuliaLang/julia/pull/42082
    frame = @static hasmethod(InferenceState, (InferenceResult,Symbol,AbstractInterpreter)) ?
            InferenceState(result, #=cache=# :global, interp)::InferenceState :
            InferenceState(result, #=cached=# true, interp)::InferenceState
    Core.Compiler.typeinf(interp, frame)
    return nothing
end

function get_specialization(@nospecialize(TT))
    match = Base._which(TT)
    mi = specialize_method(match)
    return mi
end

function get_specialization(@nospecialize(F), @nospecialize(TT))
    return get_specialization(Base.signature_type(F, TT))
end

function mkinterp(interp::AbstractInterpreter, @nospecialize(args...))
    interp′ = CthulhuInterpreter(interp)
    mi = get_specialization(args...)
    do_typeinf!(interp′, mi)
    (interp′, mi)
end

mkinterp(@nospecialize(args...); interp::AbstractInterpreter=NativeInterpreter()) = mkinterp(interp, args...)

function _descend(@nospecialize(args...);
                  interp::AbstractInterpreter=NativeInterpreter(), kwargs...)
    (interp′, mi) = mkinterp(interp, args...)
    _descend(interp′, mi; kwargs...)
end
function _descend(term::AbstractTerminal, @nospecialize(args...);
                  interp::AbstractInterpreter=NativeInterpreter(), kwargs...)
    (interp′, mi) = mkinterp(interp, args...)
    _descend(term, interp′, mi; kwargs...)
end

descend_code_typed(b::Bookmark; kw...) =
    _descend_with_error_handling(b.interp, b.mi; iswarn=false, kw...)

descend_code_warntype(b::Bookmark; kw...) =
    _descend_with_error_handling(b.interp, b.mi; iswarn=true, kw...)

FoldingTrees.writeoption(buf::IO, data::Data, charsused::Int) = FoldingTrees.writeoption(buf, data.callstr, charsused)

function ascend(term, mi; interp::AbstractInterpreter=NativeInterpreter(), kwargs...)
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
                ulocs = find_caller_of(interp, miparent, mi; allow_unspecialized=true)
                if !isempty(ulocs)
                    strlocs = [string(" "^k[3] * '"', k[2], "\", ", k[1], ": lines ", v) for (k, v) in ulocs]
                    push!(strlocs, "Browse typed code")
                    linemenu = TerminalMenus.RadioMenu(strlocs; charset=:ascii)
                    browsecodetyped = false
                    choice2 = 1
                    while choice2 != -1
                        choice2 = TerminalMenus.request(term, "\nChoose possible caller of $miparent or proceed to typed code:", linemenu; cursor=choice2)
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
            interp′ = CthulhuInterpreter(interp)
            do_typeinf!(interp′, mi)
            browsecodetyped && _descend(term, interp′, mi; iswarn=true, optimize=false, interruptexc=false, kwargs...)
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

if ccall(:jl_generating_output, Cint, ()) == 1
    input = Pipe()
    Base.link_pipe!(input, reader_supports_async=true, writer_supports_async=true)
    term = REPL.Terminals.TTYTerminal("dumb", input.out, IOBuffer(), IOBuffer())
    write(input.in, 'q')
    descend(gcd, (Int, Int); terminal=term)
    nothing
end

end
