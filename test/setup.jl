using Test, InteractiveUtils
using Cthulhu: AbstractProvider, CthulhuConfig, CthulhuState, find_method_instance, generate_code_instance, lookup, find_callsites, get_effects, Callsite, get_mi
if isdefined(parentmodule(@__MODULE__), :VSCodeServer)
    using ..VSCodeServer
end

# InteractiveUtils.@activate Compiler # use the Compiler.jl stdlib for the Base reflections too

function cthulhu_info(@nospecialize(f), @nospecialize(tt=());
                      optimize=true, interp=Cthulhu.CC.NativeInterpreter())
    provider = AbstractProvider(interp)
    mi = find_method_instance(provider, f, tt)
    ci = generate_code_instance(provider, mi)
    result = lookup(provider, ci, optimize)
    return provider, mi, ci, result
end

function find_callsites_by_ftt(@nospecialize(f), @nospecialize(TT=Tuple{}); optimize=true)
    provider, mi, ci, result = cthulhu_info(f, TT; optimize)
    callsites, _ = find_callsites(provider, result, ci)
    @test all(c -> get_effects(c) isa Cthulhu.Effects, callsites)
    return callsites
end

macro find_callsites_by_ftt(ex0...)
    return InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :find_callsites_by_ftt, ex0)
end

get_method(callsite::Callsite) = get_mi(callsite).def
