module ExternalProviderModule

using Core.IR
import ..Cthulhu
using ..Cthulhu: CC, AbstractProvider, generate_code_instance, Command, default_menu_commands, OptimizedSource, InferredSource, run_type_inference, LookupResult
using .CC: InferenceResult, AbstractInterpreter, NativeInterpreter

mutable struct ExternalOwner end

struct InferredIR
    ir::CC.IRCode
    slotnames::Vector{Symbol}
    inlining_cost::CC.InlineCostType
    nargs::UInt
    isva::Bool
end

struct ExternalInterpreter <: AbstractInterpreter
    owner::ExternalOwner
    native::NativeInterpreter
    ExternalInterpreter() = new(ExternalOwner(), NativeInterpreter())
end

Base.show(io::IO, interp::ExternalInterpreter) = print(io, typeof(interp), "()")

CC.InferenceParams(interp::ExternalInterpreter) = CC.InferenceParams(interp.native)
CC.OptimizationParams(interp::ExternalInterpreter) = CC.OptimizationParams(interp.native)
CC.get_inference_world(interp::ExternalInterpreter) = CC.get_inference_world(interp.native)
CC.get_inference_cache(interp::ExternalInterpreter) = CC.get_inference_cache(interp.native)

CC.may_optimize(::ExternalInterpreter) = true
CC.may_compress(::ExternalInterpreter) = false
CC.may_discard_trees(::ExternalInterpreter) = false
CC.method_table(interp::ExternalInterpreter) = CC.method_table(interp.native)
CC.cache_owner(interp::ExternalInterpreter) = interp.owner

function get_slotnames(def::Method)
    names = split(def.slot_syms, '\0')
    return map(Symbol, names)
end

function CC.src_inlining_policy(interp::ExternalInterpreter, @nospecialize(src), @nospecialize(info::CC.CallInfo), stmt_flag::UInt32)
    isa(src, InferredIR) && return src.inlining_cost !== CC.MAX_INLINE_COST
    return @invoke CC.src_inlining_policy(interp::AbstractInterpreter, src, info::CC.CallInfo, stmt_flag::UInt32)
end

function CC.transform_result_for_cache(interp::ExternalInterpreter, result::InferenceResult)
    (; ir) = result.src.optresult
    (; src) = result.src
    slotnames = get_slotnames(result.linfo.def)
    inlining_cost = CC.compute_inlining_cost(interp, result)
    return InferredIR(ir, slotnames, inlining_cost, src.nargs, src.isva)
end

CC.transform_result_for_cache(interp::ExternalInterpreter, result::InferenceResult, edges::CC.SimpleVector) =
    CC.transform_result_for_cache(interp, result)

function CC.transform_result_for_local_cache(interp::ExternalInterpreter, result::InferenceResult)
    CC.result_is_constabi(interp, result) && return nothing
    return CC.transform_result_for_cache(interp, result)
end

function CC.retrieve_ir_for_inlining(ci::CodeInstance, result::InferredIR)
    mi = CC.get_ci_mi(ci)
    return CC.retrieve_ir_for_inlining(mi, result.ir, true)
end

function CC.retrieve_ir_for_inlining(mi::MethodInstance, result::InferredIR, preserve_local_sources::Bool)
    return CC.retrieve_ir_for_inlining(mi, result.ir, true)
end

mutable struct ExternalProvider <: AbstractProvider
    interp::ExternalInterpreter
    ExternalProvider() = new(ExternalInterpreter())
end

CC.get_inference_world(provider::ExternalProvider) = CC.get_inference_world(provider.interp)

Cthulhu.find_method_instance(provider::ExternalProvider, @nospecialize(tt::Type{<:Tuple}), world::UInt) =
    Cthulhu.find_method_instance(provider, provider.interp, tt, world)

function Cthulhu.generate_code_instance(provider::ExternalProvider, mi::MethodInstance)
    ci = CC.typeinf_ext(provider.interp, mi, CC.SOURCE_MODE_GET_SOURCE)
    @assert isa(ci.inferred, InferredIR)
    return ci
end

function Cthulhu.LookupResult(provider::ExternalProvider, ci::CodeInstance, optimize::Bool #= ignored =#)
    @assert isa(ci.inferred, InferredIR)
    ir = copy(ci.inferred.ir)
    src = Cthulhu.ir_to_src(ir)
    src.ssavaluetypes = copy(ir.stmts.type)
    src.min_world = @atomic ci.min_world
    src.max_world = @atomic ci.max_world
    rt = Cthulhu.cached_return_type(ci)
    exct = Cthulhu.cached_exception_type(ci)
    effects = Cthulhu.get_effects(ci)
    infos = copy(ir.stmts.info)
    return LookupResult(ir, src, rt, exct, infos, src.slottypes, effects, true #= optimized =#)
end

function Cthulhu.menu_commands(provider::ExternalProvider)
    commands = default_menu_commands(provider)
    filter!(x -> !in(x.name, (:optimize, :dump_params)), commands)
    return commands
end

export ExternalProvider

end
