module Cthulhu

Base.Experimental.@compiler_options compile=min optimize=1

using CodeTracking: definition, whereis
using InteractiveUtils
using UUIDs
using REPL: REPL, AbstractTerminal
using JuliaSyntax
using JuliaSyntax: SyntaxNode, AbstractSyntaxNode, child, children
using TypedSyntax
using WidthLimitedIO

using Core: MethodInstance, MethodMatch
const CC = Core.Compiler
using .CC: Effects, EFFECTS_TOTAL, LimitedAccuracy,
    compileable_specialization, ignorelimited, specialize_method
using Base: @constprop, default_tt, isvarargtype, unwrapva, unwrap_unionall, rewrap_unionall
const mapany = Base.mapany

const ArgTypes = Vector{Any}

Base.@kwdef mutable struct CthulhuConfig
    enable_highlighter::Bool = false
    highlighter::Cmd = `pygmentize -l`
    asm_syntax::Symbol = :att
    dead_code_elimination::Bool = true
    pretty_ast::Bool = false
    interruptexc::Bool = true
    debuginfo::Symbol = :compact
    optimize::Bool = true
    iswarn::Bool = false
    hide_type_stable::Bool = false
    remarks::Bool = false
    with_effects::Bool = false
    inline_cost::Bool = false
    type_annotations::Bool = true
    annotate_source::Bool = true   # overrides optimize, although the current setting is preserved
    hide_inlay_types_vscode::Bool = false
    hide_warn_diagnostics_vscode::Bool = false
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
- `interruptexc::Bool`: Use <q>-key to quit or ascend. Defaults to `false`.
- `debuginfo::Symbol`: Initial state of "debuginfo" toggle. Defaults to `:compact`.
  Options:. `:none`, `:compact`, `:source`
- `optimize::Bool`: Initial state of "optimize" toggle. Defaults to `true`.
- `hide_type_stable::Bool`: Initial state of "hide_type_stable" toggle. Defaults to `false`.
- `iswarn::Bool`: Initial state of "warn" toggle. Defaults to `false`.
- `remarks::Bool` Initial state of "remarks" toggle. Defaults to `false`.
- `with_effects::Bool` Intial state of "effects" toggle. Defaults to `false`.
- `inline_cost::Bool` Initial state of "inlining costs" toggle. Defaults to `false`.
- `type_annotations::Bool` Initial state of "type annnotations" toggle. Defaults to `true`.
- `annotate_source::Bool` Initial state of "Source". Defaults to `true`.
- `hide_inlay_types_vscode::Bool` Initial state of "" toggle. Defaults to `false`
- `hide_warn_diagnostics_vscode::Bool` Initial state of "" toggle. Defaults to `false`
"""
const CONFIG = CthulhuConfig()

using Preferences
include("preferences.jl")
__init__() = read_config!(CONFIG)

module DInfo
    @enum DebugInfo none compact source
end
using .DInfo: DebugInfo
const AnyDebugInfo = Union{DebugInfo,Symbol}

include("interpreter.jl")
include("callsite.jl")
include("interface.jl")
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
`invoke` to descend into, select `↩`  to ascend, and press `q` or `control-c` to quit.

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
`invoke` to descend into, select `↩` to ascend, and press `q` or `control-c` to quit.

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
_descend_with_error_handling(mi::MethodInstance; kwargs...) =
    __descend_with_error_handling(mi; kwargs...)
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

descend_code_typed(interp::AbstractInterpreter, mi::MethodInstance; kwargs...) =
    _descend_with_error_handling(interp, mi; iswarn=false, kwargs...)

function codeinst_rt(code::CodeInstance)
    rettype = code.rettype
    if isdefined(code, :rettype_const)
        rettype_const = code.rettype_const
        if isa(rettype_const, Vector{Any}) && !(Vector{Any} <: rettype)
            return Core.PartialStruct(rettype, rettype_const)
        elseif isa(rettype_const, Core.PartialOpaque) && rettype <: Core.OpaqueClosure
            return rettype_const
        elseif isa(rettype_const, CC.InterConditional) && !(CC.InterConditional <: rettype)
            return rettype_const
        else
            return Const(rettype_const)
        end
    else
        return rettype
    end
end

get_effects(codeinst::CodeInstance) = CC.decode_effects(codeinst.ipo_purity_bits)
get_effects(codeinst::CodeInfo) = CC.decode_effects(codeinst.purity)
get_effects(src::InferredSource) = src.effects
get_effects(unopt::Dict{Union{MethodInstance, InferenceResult}, InferredSource}, mi::MethodInstance) =
    haskey(unopt, mi) ? get_effects(unopt[mi]) : Effects()
get_effects(result::InferenceResult) = result.ipo_effects
@static if VERSION ≥ v"1.9-"
    get_effects(result::CC.ConstPropResult) = get_effects(result.result)
    get_effects(result::CC.ConcreteResult) = result.effects
    get_effects(result::CC.SemiConcreteResult) = result.effects
else
    get_effects(result::CC.ConstResult) = result.effects
end

# `@constprop :aggressive` here in order to make sure the constant propagation of `allow_no_src`
@constprop :aggressive function lookup(interp::CthulhuInterpreter, mi::MethodInstance, optimize::Bool; allow_no_src::Bool=false)
    if optimize
        return lookup_optimized(interp, mi, allow_no_src)
    else
        return lookup_unoptimized(interp, mi)
    end
end

function lookup_optimized(interp::CthulhuInterpreter, mi::MethodInstance, allow_no_src::Bool=false)
    codeinst = interp.opt[mi]
    rt = codeinst_rt(codeinst)
    opt = codeinst.inferred
    if opt !== nothing
        opt = opt::OptimizedSource
        src = CC.copy(opt.ir)
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
        end)
        error("couldn't find the source; inspect `Main.interp` and `Main.mi`")
    end
    effects = get_effects(codeinst)
    return (; src, rt, infos, slottypes, effects, codeinf)
end

function lookup_unoptimized(interp::CthulhuInterpreter, mi::MethodInstance)
    codeinf = src = copy(interp.unopt[mi].src)
    rt = interp.unopt[mi].rt
    infos = interp.unopt[mi].stmt_info
    effects = get_effects(interp.unopt[mi])
    slottypes = src.slottypes
    if isnothing(slottypes)
        slottypes = Any[ Any for i = 1:length(src.slotflags) ]
    end
    return (; src, rt, infos, slottypes, effects, codeinf)
end

function lookup_constproped(interp::CthulhuInterpreter, override::InferenceResult, optimize::Bool)
    if optimize
        return lookup_constproped_optimized(interp, override)
    else
        return lookup_constproped_unoptimized(interp, override)
    end
end

function lookup_constproped_optimized(interp::CthulhuInterpreter, override::InferenceResult)
    opt = override.src
    if isa(opt, OptimizedSource)
        # `(override::InferenceResult).src` might has been transformed to OptimizedSource already,
        # e.g. when we switch from constant-prop' unoptimized source
        src = CC.copy(opt.ir)
        rt = override.result
        infos = src.stmts.info
        slottypes = src.argtypes
        codeinf = opt.src
        effects = opt.effects
        return (; src, rt, infos, slottypes, effects, codeinf)
    else
        # the source might be unavailable at this point,
        # when a result is fully constant-folded etc.
        return lookup(interp, override.linfo, true)
    end
end

function lookup_constproped_unoptimized(interp::CthulhuInterpreter, override::InferenceResult)
    unopt = get(interp.unopt, override, nothing)
    if unopt === nothing
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
    return (; src, rt, infos, slottypes, effects, codeinf)
end

function lookup_semiconcrete(interp::CthulhuInterpreter, override::SemiConcreteCallInfo, optimize::Bool)
    src = CC.copy(override.ir)
    rt = get_rt(override)
    infos = src.stmts.info
    slottypes = src.argtypes
    effects = get_effects(override)
    (; codeinf) = lookup(interp, get_mi(override), optimize)
    return (; src, rt, infos, slottypes, effects, codeinf)
end

function get_override(@nospecialize(info))
    isa(info, ConstPropCallInfo) && return info.result
    isa(info, SemiConcreteCallInfo) && return info
    isa(info, OCCallInfo) && return get_override(info.ci)
    return nothing
end

##
# _descend is the main driver function.
# src/reflection.jl has the tools to discover methods
# src/ui.jl provides the user facing interface to which _descend responds
##
function _descend(term::AbstractTerminal, interp::AbstractInterpreter, curs::AbstractCursor;
    override::Union{Nothing,InferenceResult,SemiConcreteCallInfo} = nothing,
    debuginfo::Union{Symbol,DebugInfo}       = CONFIG.debuginfo,                     # default is compact debuginfo
    optimize::Bool                           = CONFIG.optimize,                      # default is true
    interruptexc::Bool                       = CONFIG.interruptexc,
    iswarn::Bool                             = CONFIG.iswarn,                        # default is false
    hide_type_stable::Union{Nothing,Bool}    = CONFIG.hide_type_stable,
    verbose::Union{Nothing,Bool}             = nothing,
    remarks::Bool                            = CONFIG.remarks&!CONFIG.optimize,      # default is false
    with_effects::Bool                       = CONFIG.with_effects,                  # default is false
    inline_cost::Bool                        = CONFIG.inline_cost&CONFIG.optimize,   # default is false
    type_annotations::Bool                   = CONFIG.type_annotations,              # default is true
    annotate_source::Bool                    = CONFIG.annotate_source,               # default is true
    hide_inlay_types_vscode::Bool            = CONFIG.hide_inlay_types_vscode,       # defulat is false
    hide_warn_diagnostics_vscode::Bool       = CONFIG.hide_warn_diagnostics_vscode,  # defulat is false
    )

    if isnothing(hide_type_stable)
        hide_type_stable = something(verbose, false)
    end
    isnothing(verbose) || Base.depwarn("The `verbose` keyword argument to `Cthulhu.descend` is deprecated. Use `hide_type_stable` instead.")
    if isa(debuginfo, Symbol)
        debuginfo = getfield(DInfo, debuginfo)::DebugInfo
    end

    is_cached(key::MethodInstance) = can_descend(interp, key, optimize & !annotate_source)

    menu_options = (; cursor = '•', scroll_wrap = true)
    display_CI = true
    view_cmd = cthulhu_typed
    iostream = term.out_stream::IO
    function additional_descend(new_mi::MethodInstance)
        new_interp = CthulhuInterpreter(interp)
        do_typeinf!(new_interp, new_mi)
        _descend(term, new_interp, new_mi;
                 debuginfo, optimize, interruptexc, iswarn, hide_type_stable, remarks,
                 with_effects, inline_cost, type_annotations, annotate_source, hide_inlay_types_vscode, hide_warn_diagnostics_vscode)
    end
    custom_toggles = Cthulhu.custom_toggles(interp)
    if !(custom_toggles isa Vector{CustomToggle})
        error(lazy"""
        invalid `$AbstractInterpreter` API:
        `$(Cthulhu.custom_toggles)(interp::$(typeof(interp))` is expected to return `Vector{CustomToggle}` object.
        """)
    end
    while true
        if isa(override, InferenceResult)
            (; src, rt, infos, slottypes, codeinf, effects) = lookup_constproped(interp, curs, override, optimize & !annotate_source)
        elseif isa(override, SemiConcreteCallInfo)
                (; src, rt, infos, slottypes, codeinf, effects) = lookup_semiconcrete(interp, curs, override, optimize & !annotate_source)
        else
            if optimize && !annotate_source
                codeinst = get_optimized_codeinst(interp, curs)
                if codeinst.inferred === nothing
                    if isdefined(codeinst, :rettype_const)
                        # This was inferred to a pure constant - we have no code to show,
                        # but make something up that looks plausible.
                        callsites = Callsite[]
                        if display_CI
                            callsite = Callsite(-1, MICallInfo(codeinst.def, codeinst.rettype, get_effects(codeinst)), :invoke)
                            println(iostream)
                            println(iostream, "│ ─ $callsite")
                            println(iostream, "│  return ", Const(codeinst.rettype_const))
                            println(iostream)
                        end
                        @goto show_menu
                    else
                        @info """
                        Inference discarded the source for this call because of recursion:
                        Cthulhu nevertheless is trying to retrieve the source for further inspection.
                        """
                        additional_descend(get_mi(curs))
                        break
                    end
                end
            end
            (; src, rt, infos, slottypes, effects, codeinf) = lookup(interp, curs, optimize & !annotate_source)
        end
        mi = get_mi(curs)
        src = preprocess_ci!(src, mi, optimize & !annotate_source, CONFIG)
        if (optimize & !annotate_source) || isa(src, IRCode) # optimization might have deleted some statements
            infos = src.stmts.info
        else
            @assert length(src.code) == length(infos)
        end
        callsites, sourcenodes = find_callsites(interp, src, infos, mi, slottypes, optimize & !annotate_source, annotate_source)

        if display_CI
            pc2remarks = remarks ? get_remarks(interp, override !== nothing ? override : mi) : nothing
            pc2effects = with_effects ? get_effects(interp, override !== nothing ? override : mi) : nothing
            printstyled(IOContext(iostream, :limit=>true), mi.def, '\n'; bold=true)
            if debuginfo == DInfo.compact
                str = let debuginfo=debuginfo, src=src, codeinf=codeinf, rt=rt, mi=mi,
                          iswarn=iswarn, hide_type_stable=hide_type_stable,
                          pc2remarks=pc2remarks, pc2effects=pc2effects, inline_cost=inline_cost, type_annotations=type_annotations
                    ioctx = IOContext(iostream,
                        :color => true,
                        :displaysize => displaysize(iostream), # displaysize doesn't propagate otherwise
                        :SOURCE_SLOTNAMES => codeinf === nothing ? false : Base.sourceinfo_slotnames(codeinf),
                        :with_effects => with_effects)
                    stringify(ioctx) do lambda_io
                        cthulhu_typed(lambda_io, debuginfo, annotate_source ? codeinf : src, rt, effects, mi, callsites;
                                      iswarn, hide_type_stable,
                                      pc2remarks, pc2effects,
                                      inline_cost, type_annotations, annotate_source, hide_inlay_types_vscode, hide_warn_diagnostics_vscode,
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
                lambda_io = IOContext(iostream,
                    :SOURCE_SLOTNAMES => Base.sourceinfo_slotnames(codeinf),
                    :with_effects => with_effects)
                cthulhu_typed(lambda_io, debuginfo, src, rt, effects, mi, callsites;
                              iswarn, hide_type_stable,
                              pc2remarks, pc2effects,
                              inline_cost, type_annotations, annotate_source, hide_inlay_types_vscode, hide_warn_diagnostics_vscode,
                              interp)
            end
            view_cmd = cthulhu_typed
        else
            display_CI = true
        end

        @label show_menu

        shown_callsites = annotate_source ? sourcenodes : callsites
        menu = CthulhuMenu(shown_callsites, with_effects, optimize & !annotate_source, iswarn&get(iostream, :color, false)::Bool, hide_type_stable, custom_toggles; menu_options...)
        usg = usage(view_cmd, annotate_source, optimize, iswarn, hide_type_stable, debuginfo, remarks, with_effects, inline_cost, type_annotations, CONFIG.enable_highlighter, hide_inlay_types_vscode, hide_warn_diagnostics_vscode, custom_toggles)
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
            sourcenode = !isempty(sourcenodes) ? sourcenodes[cid] : nothing

            info = callsite.info
            if info isa MultiCallInfo
                show_sub_callsites = sub_callsites = let callsite=callsite
                    map(ci->Callsite(callsite.id, ci, callsite.head), info.callinfos)
                end
                if isempty(sub_callsites)
                    Core.eval(Main, quote
                        interp = $interp
                        mi = $mi
                        info = $info
                    end)
                    @error "Expected multiple callsites, but found none. Please fill an issue with a reproducing example."
                    continue
                end
                if sourcenode !== nothing
                    show_sub_callsites = let callsite=callsite
                        map(info.callinfos) do ci
                            p = Base.unwrap_unionall(get_mi(ci).specTypes).parameters
                            if isa(sourcenode, TypedSyntax.MaybeTypedSyntaxNode) && length(p) == length(JuliaSyntax.children(sourcenode)) + 1
                                newnode = copy(sourcenode)
                                for (i, child) in enumerate(JuliaSyntax.children(newnode))
                                    child.typ = p[i+1]
                                end
                                newnode
                            else
                                Callsite(callsite.id, ci, callsite.head)
                            end
                        end
                    end
                end
                menu = CthulhuMenu(show_sub_callsites, with_effects, optimize & !annotate_source, false, false, custom_toggles; sub_menu=true, menu_options...)
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
            if info isa UncachedCallInfo
                @info """
                Inference didn't cache this call information because of imprecise analysis due to recursion:
                Cthulhu nevertheless is trying to descend into it for further inspection.
                """
                additional_descend(get_mi(info)::MethodInstance)
                continue
            elseif info isa TaskCallInfo
                @info """
                Inference didn't analyze this call because it is a dynamic call:
                Cthulhu nevertheless is trying to descend into it for further inspection.
                """
                additional_descend(get_mi(info)::MethodInstance)
                continue
            elseif info isa RTCallInfo
                @info """
                This is a runtime call. You cannot descend into it.
                """
                @goto show_menu
            end

            # recurse
            next_cursor = navigate(curs, callsite)::Union{AbstractCursor,Nothing}
            if next_cursor === nothing
                continue
            end

            _descend(term, interp, next_cursor;
                     override = get_override(info), debuginfo,
                     optimize, interruptexc,
                     iswarn, hide_type_stable,
                     remarks, with_effects, inline_cost, type_annotations, annotate_source, hide_inlay_types_vscode, hide_warn_diagnostics_vscode)

        elseif toggle === :warn
            iswarn ⊻= true
        elseif toggle === :with_effects
            with_effects ⊻= true
        elseif toggle === :hide_type_stable
            hide_type_stable ⊻= true
        elseif toggle === :hide_inlay_types_vscode
            hide_inlay_types_vscode ⊻= true
            if TypedSyntax.inlay_hints_available()
                display(Main.VSCodeServer.InlineDisplay(false),  Dict{String, Vector{TypedSyntax.InlayHint}}())
            end
        elseif toggle === :hide_warn_diagnostics_vscode
            hide_warn_diagnostics_vscode ⊻= true
            if TypedSyntax.isvscode()
                display(Main.VSCodeServer.InlineDisplay(false), TypedSyntax.WarnUnstable[])
            end
        elseif toggle === :optimize
            optimize ⊻= true
            if !is_cached(get_mi(curs))
                @warn "Can't switch to post-optimization state, since this inference frame isn't cached."
                optimize ⊻= true
            end
            if remarks && optimize
                @warn "Disable optimization to see the inference remarks."
            end
        elseif toggle === :debuginfo
            debuginfo = DebugInfo((Int(debuginfo) + 1) % 3)
        elseif toggle === :remarks
            remarks ⊻= true
            if remarks && optimize
                @warn "Disable optimization to see the inference remarks."
            end
        elseif toggle === :inline_cost
            inline_cost ⊻= true
            if inline_cost && !optimize
                @warn "Enable optimization to see the inlining costs."
            end
        elseif toggle === :type_annotations
            type_annotations ⊻= true
        elseif toggle === :highlighter
            CONFIG.enable_highlighter ⊻= true
            if CONFIG.enable_highlighter
                @info "Using syntax highlighter $(CONFIG.highlighter)."
            else
                @info "Turned off syntax highlighter for Julia, LLVM and native code."
            end
            display_CI = false
        elseif toggle === :dump_params
            @info "Dumping inference cache."
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
                curs = update_cursor(curs, mi)
            else
                @warn "Failed to load Revise."
            end
        elseif toggle === :edit
            edit(whereis(mi.def::Method)...)
            display_CI = false
        elseif toggle === :typed
            view_cmd = cthulhu_typed
            annotate_source = false
            display_CI = true
        elseif toggle === :source
            view_cmd = cthulhu_typed
            annotate_source = true
            display_CI = true
        elseif toggle === :ast || toggle === :llvm || toggle === :native
            view_cmd = CODEVIEWS[toggle]
            println(iostream)
            view_cmd(iostream, mi, optimize, debuginfo, interp, CONFIG)
            display_CI = false
        else
            local i = findfirst(ct->ct.toggle === toggle, custom_toggles)
            @assert i !== nothing
            ct = custom_toggles[i]
            onoff = ct.onoff ⊻= true
            if onoff
                curs = ct.callback_on(curs)
            else
                curs = ct.callback_off(curs)
            end
            if !(curs isa AbstractCursor)
                local f = onoff ? "callback_on" : "callback_off"
                error(lazy"""
                invalid `$AbstractInterpreter` API:
                `f` callback is expected to return `AbstractCursor` object.
                """)
            end
        end
        println(iostream)
    end
end

function do_typeinf!(interp::AbstractInterpreter, mi::MethodInstance)
    result = InferenceResult(mi)
    # we may want to handle the case when `InferenceState(...)` returns `nothing`,
    # which indicates code generation of a `@generated` has been failed,
    # and show it in the UI in some way?
    frame = InferenceState(result, #=cache=#:global, interp)::InferenceState
    CC.typeinf(interp, frame)
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

_descend(interp::AbstractInterpreter, mi::MethodInstance; terminal=default_terminal(), kwargs...) =
    _descend(terminal, interp, mi; kwargs...)
_descend(term::AbstractTerminal, interp::AbstractInterpreter, mi::MethodInstance; kwargs...) =
    _descend(term, interp, AbstractCursor(interp, mi); kwargs...)
function _descend(@nospecialize(args...);
    interp::AbstractInterpreter=NativeInterpreter(), kwargs...)
    (interp′, mi) = mkinterp(interp, args...)
    _descend(interp′, mi; kwargs...)
end
function _descend(term::AbstractTerminal, mi::MethodInstance;
    interp::AbstractInterpreter=NativeInterpreter(), kwargs...)
    interp′ = Cthulhu.CthulhuInterpreter(interp)
    Cthulhu.do_typeinf!(interp′, mi)
    _descend(term, interp′, mi; kwargs...)
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
    root === nothing && return
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
                    explain_inlining = length(ulocs) > 1 ? "(including inlined callers represented by indentation) " : ""
                    push!(strlocs, "Browse typed code")
                    linemenu = TerminalMenus.RadioMenu(strlocs; charset=:ascii)
                    browsecodetyped = false
                    choice2 = 1
                    while choice2 != -1
                        promptstr = sprint(miparent, explain_inlining; context=:color=>get(term, :color, false)) do iobuf, mip, exi
                            printstyled(iobuf, "\nOpen an editor at a possible caller of\n  "; color=:light_cyan)
                            print(iobuf, miparent)
                            printstyled(iobuf, "\n$(explain_inlining)or browse typed code:"; color=:light_cyan)
                        end
                        choice2 = TerminalMenus.request(term, promptstr, linemenu; cursor=choice2)
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
            browsecodetyped && _descend(term, interp′, mi; annotate_source=true, iswarn=true, optimize=false, interruptexc=false, kwargs...)
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

using PrecompileTools
@setup_workload begin
    input = Base.link_pipe!(Pipe(), reader_supports_async=true, writer_supports_async=true)
    term = REPL.Terminals.TTYTerminal("dumb", input.out, devnull, devnull)
    write(input.in, 'q')
    @compile_workload begin
        descend(gcd, (Int, Int); terminal=term)
        # declare we are done with streams
        close(input.in)
    end
end

end
