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
using Core.IR
using .CC: AbstractInterpreter, CallMeta, ApplyCallInfo, CallInfo as CCCallInfo, ConstCallInfo,
    EFFECTS_TOTAL, Effects, IncrementalCompact, InferenceParams, InferenceResult,
    InferenceState, IRCode, LimitedAccuracy, MethodMatchInfo, MethodResultPure,
    NativeInterpreter, NoCallInfo, OptimizationParams, OptimizationState,
    UnionSplitApplyCallInfo, UnionSplitInfo, WorldRange, WorldView, get_inference_world,
    argextype, argtypes_to_type, compileable_specialization, ignorelimited, singleton_type,
    specialize_method, sptypes_from_meth_instance, widenconst, method_table, findsup
using Base: @constprop, default_tt, isvarargtype, unwrapva, unwrap_unionall, rewrap_unionall
const mapany = Base.mapany

const ArgTypes = Vector{Any}

using Base: get_world_counter

get_mi(ci::CodeInstance) = CC.get_ci_mi(ci)
get_mi(mi::MethodInstance) = mi

Base.@kwdef mutable struct CthulhuConfig
    enable_highlighter::Bool = false
    highlighter::Cmd = `pygmentize -l`
    asm_syntax::Symbol = :att
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

module DInfo
    @enum DebugInfo none compact source
end
using .DInfo: DebugInfo
const AnyDebugInfo = Union{DebugInfo,Symbol}

include("callsite.jl")
include("interface.jl")
include("interpreter.jl")
include("provider.jl")
include("reflection.jl")
include("ui.jl")
include("codeview.jl")
include("backedges.jl")
include("descend.jl")
include("ascend.jl")

"""
    @interp

For debugging. Returns a CthulhuInterpreter from the appropriate entrypoint.
"""
macro interp(ex0...)
    InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :mkinterp, ex0)
end

using .CC: cached_return_type

cached_exception_type(code::CodeInstance) = code.exctype

get_effects(codeinst::CodeInstance) = CC.decode_effects(codeinst.ipo_purity_bits)
get_effects(codeinst::CodeInfo) = CC.decode_effects(codeinst.purity)
get_effects(result::InferenceResult) = result.ipo_effects
get_effects(source::InferredSource) = source.effects
get_effects(result::CC.ConstPropResult) = get_effects(result.result)
get_effects(result::CC.ConcreteResult) = result.effects
get_effects(result::CC.SemiConcreteResult) = result.effects

get_specialization(@nospecialize(f), @nospecialize(tt=default_tt(f))) =
    get_specialization(Base.signature_type(f, tt))
get_specialization(@nospecialize tt::Type{<:Tuple}) =
    specialize_method(Base._which(tt))

function mkinterp(interp::AbstractInterpreter, @nospecialize(args...))
    interp′ = CthulhuInterpreter(interp)
    mi = get_specialization(args...)
    ci = run_type_inference(interp′, mi)
    return interp′, ci
end
mkinterp(@nospecialize(args...); interp::AbstractInterpreter=NativeInterpreter()) = mkinterp(interp, args...)

_descend(term::AbstractTerminal, provider::AbstractProvider, ci::CodeInstance; kwargs...) =
    _descend(term, provider, AbstractCursor(provider, ci); kwargs...)
function _descend(term::AbstractTerminal, provider::AbstractProvider, mi::MethodInstance; kwargs...)
    ci = generate_code_instance(provider, mi)
    _descend(term, provider, ci)
end
function _descend(term::AbstractTerminal, provider::AbstractProvider, @nospecialize(args...); kwargs...)
    mi = find_method_instance(provider, args...)
    isa(mi, MethodInstance) || error("No method instance found for $(join(args, ", "))")
    _descend(term, provider, mi; kwargs...)
end

function _descend(term::AbstractTerminal, interp::AbstractInterpreter, @nospecialize(args...); kwargs...)
    provider = DefaultProvider(interp)
    _descend(term, provider, args...; kwargs...)
end

_descend(term::AbstractTerminal, @nospecialize(args...); kwargs...) =
    _descend(term, NativeInterpreter(), args...)
_descend(@nospecialize(args...); terminal=default_terminal(), kwargs...) =
    _descend(terminal, args...; kwargs...)

descend_code_typed_impl(b::Bookmark; kw...) =
    _descend_with_error_handling(b.interp, b.ci.def; iswarn=false, kw...)
descend_code_warntype_impl(b::Bookmark; kw...) =
    _descend_with_error_handling(b.interp, b.ci.def; iswarn=true, kw...)

FoldingTrees.writeoption(buf::IO, data::Data, charsused::Int) = FoldingTrees.writeoption(buf, data.callstr, charsused)
