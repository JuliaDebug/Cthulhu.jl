Base.Experimental.@compiler_options compile=min optimize=1

using Accessors
using CodeTracking: CodeTracking, definition, whereis, maybe_fix_path
using InteractiveUtils
using UUIDs
using REPL: REPL, AbstractTerminal
using JuliaSyntax
using JuliaSyntax: SyntaxNode, AbstractSyntaxNode, children, is_leaf
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

using Preferences
include("config.jl")
include("preferences.jl")

include("interface.jl")
include("callsite.jl")
include("compiler.jl")
include("state.jl")
include("interpreter.jl")
include("provider.jl")
include("reflection.jl")
include("ui.jl")
include("codeview.jl")
include("bookmark.jl")
include("descend.jl")
include("ascend.jl")

resolve_module(::AbstractProvider) = @__MODULE__

using .CC: cached_return_type

cached_exception_type(code::CodeInstance) = code.exctype

get_effects(codeinst::CodeInstance) = CC.decode_effects(codeinst.ipo_purity_bits)
get_effects(codeinst::CodeInfo) = CC.decode_effects(codeinst.purity)
get_effects(result::InferenceResult) = result.ipo_effects
get_effects(source::InferredSource) = source.effects
get_effects(result::CC.ConstPropResult) = get_effects(result.result)
get_effects(result::CC.ConcreteResult) = result.effects
get_effects(result::CC.SemiConcreteResult) = result.effects
