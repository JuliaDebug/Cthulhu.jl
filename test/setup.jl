using Test, Cthulhu, InteractiveUtils
if isdefined(parentmodule(@__MODULE__), :VSCodeServer)
    using ..VSCodeServer
end

# InteractiveUtils.@activate Compiler # use the Compiler.jl stdlib for the Base reflections too

function cthulhu_info(@nospecialize(f), @nospecialize(tt=());
                      optimize=true, interp=Cthulhu.CC.NativeInterpreter())
    (interp, codeinst) = Cthulhu.mkinterp(f, tt; interp)
    (; src, rt, exct, infos, slottypes, effects) =
        Cthulhu.LookupResult(interp, codeinst, optimize)
    return (; interp, src, infos, codeinst, rt, exct, slottypes, effects)
end

function find_callsites_by_ftt(@nospecialize(f), @nospecialize(TT=Tuple{}); optimize=true)
    (; interp, src, infos, codeinst, slottypes) = cthulhu_info(f, TT; optimize)
    src === nothing && return Cthulhu.Callsite[]
    callsites, _ = Cthulhu.find_callsites(interp, src, infos, codeinst, slottypes)
    @test all(c -> Cthulhu.get_effects(c) isa Cthulhu.Effects, callsites)
    return callsites
end

macro find_callsites_by_ftt(ex0...)
    return InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :find_callsites_by_ftt, ex0)
end

get_mi(callsite::Cthulhu.Callsite) = Cthulhu.get_ci(callsite).def
get_method(callsite::Cthulhu.Callsite) = get_mi(callsite).def
