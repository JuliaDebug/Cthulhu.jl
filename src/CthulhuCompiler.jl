Base.Experimental.@compiler_options compile=min optimize=1

import .Cthulhu:
    AbstractProvider, cthulhu_ast, cthulhu_llvm, cthulhu_native, cthulhu_source,
    cthulhu_typed, find_caller_of, find_callsites, find_method_instance,
    generate_code_instance, get_abstract_interpreter, get_ci, get_inference_world,
    get_inlining_costs, get_override, get_pc_effects, get_pc_excts, get_pc_remarks, get_rt,
    ir_to_src, lookup, print_callsite_info, show_callinfo, show_callsite, show_parameters
using .Cthulhu:
    CthulhuState, CthulhuConfig, CallInfo, Callsite,
    cached_exception_type, get_mi

using Base: get_world_counter, isvarargtype, mapany, unwrap_unionall, unwrapva
using JuliaSyntax: JuliaSyntax, children, is_leaf

using .CC: AbstractInterpreter, ApplyCallInfo, CallInfo as CCCallInfo, CallMeta,
    EFFECTS_TOTAL, Effects, IncrementalCompact, InferenceParams,
    InferenceResult, InferenceState, IRCode, LimitedAccuracy, MethodMatchInfo,
    MethodResultPure, NativeInterpreter, NoCallInfo, OptimizationParams, OptimizationState,
    UnionSplitApplyCallInfo, UnionSplitInfo,
    argextype, argtypes_to_type, cached_return_type, compileable_specialization, findsup,
    ignorelimited, method_table, singleton_type, specialize_method, sptypes_from_meth_instance,
    widenconst

const ArgTypes = Vector{Any}

include("compiler/callsite.jl")
include("compiler/interface.jl")
include("compiler/lookup.jl")
include("compiler/interpreter.jl")
include("compiler/provider.jl")
include("compiler/reflection.jl")
include("compiler/codeview.jl")

get_effects(codeinst::CodeInstance) = CC.decode_effects(codeinst.ipo_purity_bits)
get_effects(codeinst::CodeInfo) = CC.decode_effects(codeinst.purity)
get_effects(result::InferenceResult) = result.ipo_effects
get_effects(source::InferredSource) = source.effects
@static if VERSION < v"1.14.0-DEV.60"
get_effects(result::CC.ConstPropResult) = get_effects(result.result)
end
get_effects(result::CC.ConcreteResult) = result.effects
get_effects(result::CC.SemiConcreteResult) = result.effects
