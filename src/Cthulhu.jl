module Cthulhu

Base.Experimental.@compiler_options compile=min optimize=1

using CodeTracking: CodeTracking, definition, whereis, maybe_fix_path
using InteractiveUtils
using UUIDs
using REPL: REPL, AbstractTerminal
using JuliaSyntax
using JuliaSyntax: SyntaxNode, AbstractSyntaxNode, child, children
using TypedSyntax
using WidthLimitedIO

using Core: MethodInstance, MethodMatch
@static if VERSION ≥ v"1.12.0-DEV.1581"
    using Compiler: Compiler as CC
    using Compiler.IRShow: IRShow
else
    const CC = Core.Compiler
    const IRShow = Base.IRShow
end
using Core.IR
using .CC: AbstractInterpreter, ApplyCallInfo, CallInfo as CCCallInfo, ConstCallInfo,
    EFFECTS_TOTAL, Effects, IncrementalCompact, InferenceParams, InferenceResult,
    InferenceState, IRCode, LimitedAccuracy, MethodMatchInfo, MethodResultPure,
    NativeInterpreter, NoCallInfo, OptimizationParams, OptimizationState,
    UnionSplitApplyCallInfo, UnionSplitInfo, WorldRange, WorldView,
    argextype, argtypes_to_type, compileable_specialization, ignorelimited, singleton_type,
    specialize_method, sptypes_from_meth_instance, widenconst
using Base: @constprop, default_tt, isvarargtype, unwrapva, unwrap_unionall, rewrap_unionall
const mapany = Base.mapany

const ArgTypes = Vector{Any}

@static if VERSION ≥ v"1.11.0-DEV.1498"
    import .CC: get_inference_world
    using Base: get_world_counter
else
    import .CC: get_world_counter, get_world_counter as get_inference_world
end

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
    exception_type::Bool = false
    inline_cost::Bool = false
    type_annotations::Bool = true
    annotate_source::Bool = true   # overrides optimize, although the current setting is preserved
    inlay_types_vscode::Bool = true
    diagnostics_vscode::Bool = true
    jump_always::Bool = false
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
- `exception_type::Bool` `Intial state of "exception type" toggle. Defaults to `false`.
- `inline_cost::Bool` Initial state of "inlining costs" toggle. Defaults to `false`.
- `type_annotations::Bool` Initial state of "type annnotations" toggle. Defaults to `true`.
- `annotate_source::Bool` Initial state of "Source". Defaults to `true`.
- `inlay_types_vscode::Bool` Initial state of "vscode: inlay types" toggle. Defaults to `true`
- `diagnostics_vscode::Bool` Initial state of "Vscode: diagnostics" toggle. Defaults to `true`
- `jump_always::Bool` Initial state of "jump to source always" toggle. Defaults to `false`.
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
types, and calls [`descend_code_typed`](@ref) on the resulting expression.
See [`Cthulhu.CONFIG`](@ref) for options and their defaults.

# Examples
```julia
julia> @descend_code_typed sin(1)
[...]

julia> @descend_code_typed optimize=false sin(1)
[...]
```
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
types, and calls [`descend_code_warntype`](@ref) on the resulting expression.
See [`Cthulhu.CONFIG`](@ref) for options and their defaults.

# Examples
```julia
julia> function foo()
           T = rand() > 0.5 ? Int64 : Float64
           sin(rand(T))
       end
foo (generic function with 1 method)

julia> @descend_code_warntype foo()
[...]

julia> @descend_code_warntype hide_type_stable=true foo()
[...]
```
"""
macro descend_code_warntype(ex0...)
    InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :descend_code_warntype, ex0)
end

"""
    @descend

Evaluates the arguments to the function or macro call, determines their
types, and calls [`descend`](@ref) on the resulting expression.
See [`Cthulhu.CONFIG`](@ref) for options and their defaults.

# Examples
```julia
julia> @descend sin(1)
[...]

julia> @descend iswarn=false foo()
[...]
```
"""
macro descend(ex0...)
    InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :descend, ex0)
end

"""
    descend_code_typed(f, argtypes=Tuple{...}; kwargs...)
    descend_code_typed(tt::Type{<:Tuple}; kwargs...)
    descend_code_typed(Cthulhu.BOOKMARKS[i]; kwargs...)
    descend_code_typed(mi::MethodInstance; kwargs...)

Given a function and a tuple-type, interactively explore the output of
`code_typed` by descending into `invoke` statements. Type enter to select an
`invoke` to descend into, select `↩`  to ascend, and press `q` or `control-c` to quit.
See [`Cthulhu.CONFIG`](@ref) for `kwargs` and their defaults.

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
           sin(rand(T))
       end
[...]
```
"""
descend_code_typed(@nospecialize(args...); kwargs...) =
    _descend_with_error_handling(args...; annotate_source=false, iswarn=false, kwargs...)

"""
    descend_code_warntype(f, argtypes=Tuple{...}; kwargs...)
    descend_code_warntype(tt::Type{<:Tuple}; kwargs...)
    descend_code_warntype(Cthulhu.BOOKMARKS[i])
    descend_code_warntype(mi::MethodInstance; kwargs...)

Given a function and a tuple-type, interactively explore the output of
`code_warntype` by descending into `invoke` statements. Type enter to select an
`invoke` to descend into, select `↩` to ascend, and press `q` or `control-c` to quit.
See [`Cthulhu.CONFIG`](@ref) for `kwargs` and their defaults.

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
           sin(rand(T))
       end
[...]
```
"""
descend_code_warntype(@nospecialize(args...); kwargs...) =
    _descend_with_error_handling(args...; annotate_source=false, iswarn=true, optimize=false, kwargs...)

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
        TypedSyntax.clear_all_vscode()
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
    descend(f, argtypes=Tuple{...}; kwargs...)
    descend(tt::Type{<:Tuple}; kwargs...)
    descend(Cthulhu.BOOKMARKS[i])
    descend(mi::MethodInstance; kwargs...)

Given a function and a tuple-type, interactively explore the source code of functions
annotated with inferred types by descending into `invoke` statements. Type enter to select an
`invoke` to descend into, select `↩` to ascend, and press `q` or `control-c` to quit.
See [`Cthulhu.CONFIG`](@ref) for `kwargs` and their defaults.

# Usage:
```julia
julia> descend(sin, (Int,))
[...]

julia> descend(sin, Tuple{Int})
[...]

julia> descend(Tuple{typeof(sin), Int})
[...]

julia> descend() do
           T = rand() > 0.5 ? Int64 : Float64
           sin(rand(T))
       end
[...]
```
"""
descend(@nospecialize(args...); kwargs...) =
    _descend_with_error_handling(args...; iswarn=true, kwargs...)

@static if VERSION ≥ v"1.11.0-DEV.207"
    using .CC: cached_return_type
else
function cached_return_type(code::CodeInstance)
    rettype = code.rettype
    isdefined(code, :rettype_const) || return rettype
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
end
end

function cached_exception_type(code::CodeInstance)
    @static if VERSION ≥ v"1.11.0-DEV.945"
        return code.exctype
    else
        return nothing
    end
end

get_effects(codeinst::CodeInstance) = CC.decode_effects(codeinst.ipo_purity_bits)
get_effects(codeinst::CodeInfo) = CC.decode_effects(codeinst.purity)
get_effects(result::InferenceResult) = result.ipo_effects
get_effects(result::CC.ConstPropResult) = get_effects(result.result)
get_effects(result::CC.ConcreteResult) = result.effects
get_effects(result::CC.SemiConcreteResult) = result.effects

struct LookupResult
    src::Union{CodeInfo,IRCode}
    rt
    exct
    infos::Vector{CCCallInfo}
    slottypes::Vector{Any}
    effects::Effects
    codeinf::Union{Nothing,CodeInfo}
    function LookupResult(src::Union{CodeInfo,IRCode}, @nospecialize(rt), @nospecialize(exct),
                          infos::Vector{CCCallInfo}, slottypes::Vector{Any},
                          effects::Effects, codeinf::Union{Nothing,CodeInfo})
        return new(src, rt, exct, infos, slottypes, effects, codeinf)
    end
end

# `@constprop :aggressive` here in order to make sure the constant propagation of `allow_no_src`
@constprop :aggressive function lookup(interp::CthulhuInterpreter, ci::CodeInstance, optimize::Bool; allow_no_src::Bool=false)
    if optimize
        return lookup_optimized(interp, ci, allow_no_src)
    else
        return lookup_unoptimized(interp, ci)
    end
end

function lookup_optimized(interp::CthulhuInterpreter, ci::CodeInstance, allow_no_src::Bool=false)
    rt = cached_return_type(ci)
    exct = cached_exception_type(ci)
    opt = ci.inferred
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
        slottypes = Any[Base.unwrap_unionall(ci.def.specTypes).parameters...]
    else
        Core.eval(Main, quote
            interp = $interp
            ci = $ci
        end)
        error("couldn't find the source; inspect `Main.interp` and `Main.mi`")
    end
    effects = get_effects(ci)
    return LookupResult(src, rt, exct, infos, slottypes, effects, codeinf)
end

function lookup_unoptimized(interp::CthulhuInterpreter, ci::CodeInstance)
    unopt = interp.unopt[ci]
    codeinf = src = copy(unopt.src)
    (; rt, exct) = unopt
    infos = unopt.stmt_info
    effects = unopt.effects
    slottypes = src.slottypes
    if isnothing(slottypes)
        slottypes = Any[ Any for i = 1:length(src.slotflags) ]
    end
    return LookupResult(src, rt, exct, infos, slottypes, effects, codeinf)
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
    isa(opt, OptimizedSource) || error("couldn't find the source")
    # `(override::InferenceResult).src` might has been transformed to OptimizedSource already,
    # e.g. when we switch from constant-prop' unoptimized source
    src = CC.copy(opt.ir)
    rt = override.result
    exct = @static hasfield(InferenceResult, :exc_result) ? override.exc_result : nothing
    infos = src.stmts.info
    slottypes = src.argtypes
    codeinf = opt.src
    effects = opt.effects
    return LookupResult(src, rt, exct, infos, slottypes, effects, codeinf)
end

function lookup_constproped_unoptimized(interp::CthulhuInterpreter, override::InferenceResult)
    unopt = interp.unopt[override]
    codeinf = src = copy(unopt.src)
    (; rt, exct) = unopt
    infos = unopt.stmt_info
    effects = get_effects(unopt)
    slottypes = src.slottypes
    if isnothing(slottypes)
        slottypes = Any[ Any for i = 1:length(src.slotflags) ]
    end
    return LookupResult(src, rt, exct, infos, slottypes, effects, codeinf)
end

function lookup_semiconcrete(interp::CthulhuInterpreter, override::SemiConcreteCallInfo, optimize::Bool)
    src = CC.copy(override.ir)
    rt = get_rt(override)
    exct = Any # TODO
    infos = src.stmts.info
    slottypes = src.argtypes
    effects = get_effects(override)
    codeinf = nothing # TODO try to find `CodeInfo` for the regular inference?
    return LookupResult(src, rt, exct, infos, slottypes, effects, codeinf)
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
    debuginfo::Union{Symbol,DebugInfo}    = CONFIG.debuginfo,                     # default is compact debuginfo
    optimize::Bool                        = CONFIG.optimize,                      # default is true
    interruptexc::Bool                    = CONFIG.interruptexc,
    iswarn::Bool                          = CONFIG.iswarn,                        # default is false
    hide_type_stable::Union{Nothing,Bool} = CONFIG.hide_type_stable,
    verbose::Union{Nothing,Bool}          = nothing,
    remarks::Bool                         = CONFIG.remarks&!CONFIG.optimize,      # default is false
    with_effects::Bool                    = CONFIG.with_effects,                  # default is false
    exception_type::Bool                  = CONFIG.exception_type,                # default is false
    inline_cost::Bool                     = CONFIG.inline_cost&CONFIG.optimize,   # default is false
    type_annotations::Bool                = CONFIG.type_annotations,              # default is true
    annotate_source::Bool                 = CONFIG.annotate_source,               # default is true
    inlay_types_vscode::Bool              = CONFIG.inlay_types_vscode,            # default is true
    diagnostics_vscode::Bool              = CONFIG.diagnostics_vscode,            # default is true
    jump_always::Bool                     = CONFIG.jump_always,                   # default is false
    )

    if isnothing(hide_type_stable)
        hide_type_stable = something(verbose, false)
    end
    isnothing(verbose) || Base.depwarn("The `verbose` keyword argument to `Cthulhu.descend` is deprecated. Use `hide_type_stable` instead.")
    if isa(debuginfo, Symbol)
        debuginfo = getfield(DInfo, debuginfo)::DebugInfo
    end

    menu_options = (; cursor = '•', scroll_wrap = true)
    display_CI = true
    view_cmd = cthulhu_typed
    iostream = term.out_stream::IO
    function additional_descend(new_mi::MethodInstance)
        new_interp = CthulhuInterpreter(interp)
        do_typeinf!(new_interp, new_mi)
        _descend(term, new_interp, new_mi;
                 debuginfo, optimize, interruptexc, iswarn, hide_type_stable, remarks,
                 with_effects, exception_type,
                 inline_cost, type_annotations, annotate_source,
                 inlay_types_vscode, diagnostics_vscode)
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
            (; src, rt, exct, infos, slottypes, codeinf, effects) = lookup_constproped(interp, curs, override, optimize & !annotate_source)
        elseif isa(override, SemiConcreteCallInfo)
                (; src, rt, exct, infos, slottypes, codeinf, effects) = lookup_semiconcrete(interp, curs, override, optimize & !annotate_source)
        else
            if optimize && !annotate_source
                codeinst = curs.ci
                if codeinst.inferred === nothing
                    if isdefined(codeinst, :rettype_const)
                        # TODO use `codeinfo_for_const`
                        # This was inferred to a pure constant - we have no code to show,
                        # but make something up that looks plausible.
                        callsites = Callsite[]
                        if display_CI
                            exct = @static VERSION ≥ v"1.11.0-DEV.945" ? codeinst.exct : nothing
                            callsite = Callsite(-1, EdgeCallInfo(codeinst, codeinst.rettype, get_effects(codeinst), exct), :invoke)
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
            (; src, rt, exct, infos, slottypes, effects, codeinf) = lookup(interp, curs, optimize & !annotate_source)
        end
        ci = get_ci(curs)
        mi = ci.def
        src = preprocess_ci!(src, mi, optimize & !annotate_source, CONFIG)
        if (optimize & !annotate_source) || isa(src, IRCode) # optimization might have deleted some statements
            infos = src.stmts.info
        else
            @assert length(src.code) == length(infos)
        end
        infkey = override isa InferenceResult ? override : ci
        @static if VERSION ≥ v"1.11.0-DEV.1127"
        pc2excts = exception_type ? get_pc_exct(interp, infkey) : nothing
        else
        if exception_type
            @warn "Statement-wise and call-wise exception type information is available only on v\"1.11.0-DEV.1127\" and later"
        end
        pc2excts = nothing
        end
        callsites, sourcenodes = find_callsites(interp, src, infos, mi, slottypes, optimize & !annotate_source, annotate_source, pc2excts)

        if jump_always
            if isdefined(Main, :VSCodeServer) && Main.VSCodeServer isa Module && isdefined(Main.VSCodeServer, :openfile)
                Main.VSCodeServer.openfile(whereis(mi.def::Method)...; preserve_focus=true)
            else
                edit(whereis(mi.def::Method)...)
            end
        end

        if display_CI
            pc2remarks = remarks ? get_pc_remarks(interp, infkey) : nothing
            pc2effects = with_effects ? get_pc_effects(interp, infkey) : nothing
            printstyled(IOContext(iostream, :limit=>true), mi.def, '\n'; bold=true)
            if debuginfo == DInfo.compact
                str = let debuginfo=debuginfo, src=src, codeinf=codeinf, rt=rt,
                          iswarn=iswarn, hide_type_stable=hide_type_stable,
                          pc2remarks=pc2remarks, pc2effects=pc2effects, pc2excts=pc2excts,
                          inline_cost=inline_cost, type_annotations=type_annotations
                    ioctx = IOContext(iostream,
                        :color => true,
                        :displaysize => displaysize(iostream), # displaysize doesn't propagate otherwise
                        :SOURCE_SLOTNAMES => codeinf === nothing ? false : Base.sourceinfo_slotnames(codeinf),
                        :with_effects => with_effects,
                        :exception_type => exception_type)
                    stringify(ioctx) do lambda_io
                        cthulhu_typed(lambda_io, debuginfo, annotate_source ? codeinf : src, rt, exct, effects, ci;
                                      iswarn, optimize, hide_type_stable,
                                      pc2remarks, pc2effects, pc2excts,
                                      inline_cost, type_annotations, annotate_source, inlay_types_vscode, diagnostics_vscode,
                                      jump_always, interp)
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
                    :SOURCE_SLOTNAMES => codeinf === nothing ? false : Base.sourceinfo_slotnames(codeinf),
                    :with_effects => with_effects,
                    :exception_type => exception_type)
                cthulhu_typed(lambda_io, debuginfo, src, rt, exct, effects, ci;
                              iswarn, optimize, hide_type_stable,
                              pc2remarks, pc2effects, pc2excts,
                              inline_cost, type_annotations, annotate_source, inlay_types_vscode, diagnostics_vscode,
                              jump_always, interp)
            end
            view_cmd = cthulhu_typed
        else
            display_CI = true
        end

        @label show_menu

        shown_callsites = annotate_source ? sourcenodes : callsites
        menu = CthulhuMenu(shown_callsites, with_effects, exception_type,
                           optimize & !annotate_source,
                           iswarn&get(iostream, :color, false)::Bool,
                           hide_type_stable, custom_toggles; menu_options...)
        usg = usage(view_cmd, annotate_source, optimize, iswarn, hide_type_stable,
                    debuginfo, remarks, with_effects, exception_type, inline_cost,
                    type_annotations, CONFIG.enable_highlighter, inlay_types_vscode,
                    diagnostics_vscode, jump_always, custom_toggles)
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
                menu = CthulhuMenu(show_sub_callsites, with_effects, exception_type,
                                   optimize & !annotate_source, false, false, custom_toggles;
                                   sub_menu=true, menu_options...)
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
            if info isa TaskCallInfo
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
                     remarks, with_effects, exception_type, inline_cost,
                     type_annotations, annotate_source,
                     inlay_types_vscode, diagnostics_vscode,
                     jump_always)

        elseif toggle === :warn
            iswarn ⊻= true
        elseif toggle === :with_effects
            with_effects ⊻= true
        elseif toggle === :exception_type
            exception_type ⊻= true
        elseif toggle === :hide_type_stable
            hide_type_stable ⊻= true
        elseif toggle === :inlay_types_vscode
            inlay_types_vscode ⊻= true
            TypedSyntax.clear_inlay_hints_vscode()
        elseif toggle === :diagnostics_vscode
            diagnostics_vscode ⊻= true
            TypedSyntax.clear_diagnostics_vscode()
        elseif toggle === :optimize
            optimize ⊻= true
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
                ci = do_typeinf!(interp, mi)
                curs = update_cursor(curs, ci)
            else
                @warn "Failed to load Revise."
            end
        elseif toggle === :edit
            edit(whereis(mi.def::Method)...)
            display_CI = false
        elseif toggle === :jump_always
            jump_always ⊻= true
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
            world = get_inference_world(interp)
            println(iostream)
            @static if VERSION < v"1.12.0-DEV.669"
                view_cmd(iostream, mi, optimize, debuginfo, world, CONFIG)
            else
                src = CC.typeinf_code(interp, mi, true)
                view_cmd(iostream, mi, src, optimize, debuginfo, world, CONFIG)
            end
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

    TypedSyntax.clear_all_vscode()
end

function do_typeinf!(interp::AbstractInterpreter, mi::MethodInstance)
    result = InferenceResult(mi)
    ci = CC.engine_reserve(interp, mi)
    result.ci = ci
    # we may want to handle the case when `InferenceState(...)` returns `nothing`,
    # which indicates code generation of a `@generated` has been failed,
    # and show it in the UI in some way?
    frame = InferenceState(result, #=cache_mode=#:global, interp)::InferenceState
    CC.typeinf(interp, frame)
    return ci
end

get_specialization(@nospecialize(f), @nospecialize(tt=default_tt(f))) =
    get_specialization(Base.signature_type(f, tt))
get_specialization(@nospecialize tt::Type{<:Tuple}) =
    specialize_method(Base._which(tt))

function mkinterp(interp::AbstractInterpreter, @nospecialize(args...))
    interp′ = CthulhuInterpreter(interp)
    mi = get_specialization(args...)
    ci = do_typeinf!(interp′, mi)
    return interp′, ci
end
mkinterp(@nospecialize(args...); interp::AbstractInterpreter=NativeInterpreter()) = mkinterp(interp, args...)

function _descend(@nospecialize(args...);
    interp::AbstractInterpreter=NativeInterpreter(), kwargs...)
    (interp′, ci) = mkinterp(interp, args...)
    _descend(interp′, ci; kwargs...)
end
_descend(interp::AbstractInterpreter, ci::CodeInstance; terminal=default_terminal(), kwargs...) =
    _descend(terminal, interp, ci; kwargs...)
_descend(term::AbstractTerminal, interp::AbstractInterpreter, ci::CodeInstance; kwargs...) =
    _descend(term, interp, AbstractCursor(interp, ci); kwargs...)
function _descend(term::AbstractTerminal, mi::MethodInstance;
    interp::AbstractInterpreter=NativeInterpreter(), kwargs...)
    interp′ = Cthulhu.CthulhuInterpreter(interp)
    ci = Cthulhu.do_typeinf!(interp′, mi)
    _descend(term, interp′, ci; kwargs...)
end
function _descend(term::AbstractTerminal, @nospecialize(args...);
    interp::AbstractInterpreter=NativeInterpreter(), kwargs...)
    (interp′, ci) = mkinterp(interp, args...)
    _descend(term, interp′, ci; kwargs...)
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
                    ulocs = [(k[1], maybe_fix_path(String(k[2])), k[3]) => v for (k, v) in ulocs]
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
                            loc, lines = ulocs[choice2]
                            edit(loc[2], first(lines))
                        elseif choice2 == length(strlocs)
                            browsecodetyped = true
                            break
                        end
                    end
                end
            end
            if !isa(mi, MethodInstance)
                error("You can only descend into known calls. If you tried to descend into a runtime-dispatched signature, try its caller instead.")
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
    try
        input = Base.link_pipe!(Pipe(), reader_supports_async=true, writer_supports_async=true)
        term = REPL.Terminals.TTYTerminal("dumb", input.out, devnull, devnull)
        write(input.in, 'q')
        @compile_workload begin
            descend(gcd, (Int, Int); terminal=term)
            # declare we are done with streams
            close(input.in)
        end
    catch err
        @error "Errorred while running the precompile workload, the package may or may not work but latency will be long" exeption=(err,catch_backtrace())
    end
end

end
