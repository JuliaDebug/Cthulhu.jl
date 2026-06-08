Base.Experimental.@compiler_options compile=min optimize=1

import .Cthulhu: AbstractProvider, get_abstract_interpreter, get_inference_world, find_method_instance, generate_code_instance, get_override, lookup, find_caller_of, get_inlining_costs, show_parameters, get_ci, get_rt, get_pc_remarks, get_pc_effects, get_pc_excts, show_callsite, show_callinfo, print_callsite_info, cthulhu_source, cthulhu_typed, cthulhu_ast, cthulhu_llvm, cthulhu_native, find_callsites, ir_to_src
using .Cthulhu: CthulhuState, CthulhuConfig, CallInfo, Callsite, cached_exception_type, get_mi

using Base: isvarargtype, unwrapva, unwrap_unionall, mapany, get_world_counter
using JuliaSyntax: JuliaSyntax, children, is_leaf

using .CC: AbstractInterpreter, CallMeta, ApplyCallInfo, CallInfo as CCCallInfo,
    EFFECTS_TOTAL, Effects, IncrementalCompact, InferenceParams, InferenceResult,
    InferenceState, IRCode, LimitedAccuracy, MethodMatchInfo, MethodResultPure,
    NativeInterpreter, NoCallInfo, OptimizationParams, OptimizationState,
    UnionSplitApplyCallInfo, UnionSplitInfo, WorldRange,
    argextype, argtypes_to_type, compileable_specialization, ignorelimited, singleton_type,
    specialize_method, sptypes_from_meth_instance, widenconst, method_table, findsup,
    cached_return_type

# `ConstCallInfo` was removed on Julia 1.14 (#59413): const-prop results now live directly
# in `MethodMatchInfo.call_results`. Use a placeholder so `isa` checks compile on both.
@static if isdefined(CC, :ConstCallInfo)
    const ConstCallInfoT = CC.ConstCallInfo
else
    struct _NoConstCallInfo end
    const ConstCallInfoT = _NoConstCallInfo
end

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
@static if isdefined(CC, :ConstPropResult)
    get_effects(result::CC.ConstPropResult) = get_effects(result.result)
end
get_effects(result::CC.ConcreteResult) = result.effects
get_effects(result::CC.SemiConcreteResult) = result.effects
