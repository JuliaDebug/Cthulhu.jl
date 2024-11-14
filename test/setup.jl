using Test, Cthulhu, InteractiveUtils
if isdefined(parentmodule(@__MODULE__), :VSCodeServer)
    using ..VSCodeServer
end

@static if VERSION ≥ v"1.12.0-DEV.1581"
InteractiveUtils.@activate Compiler # use the Compiler.jl stdlib for the Base reflections too
end

function cthulhu_info(@nospecialize(f), @nospecialize(tt=());
                      optimize=true, interp=Cthulhu.CC.NativeInterpreter())
    (interp, mi) = Cthulhu.mkinterp(f, tt; interp)
    (; src, rt, exct, infos, slottypes, effects) =
        Cthulhu.lookup(interp, mi, optimize; allow_no_src=true)
    if src !== nothing
        config = Cthulhu.CthulhuConfig(; dead_code_elimination=true)
        src = Cthulhu.preprocess_ci!(src, mi, optimize, config)
    end
    return (; interp, src, infos, mi, rt, exct, slottypes, effects)
end

function find_callsites_by_ftt(@nospecialize(f), @nospecialize(TT=Tuple{}); optimize=true)
    (; interp, src, infos, mi, slottypes) = cthulhu_info(f, TT; optimize)
    src === nothing && return Cthulhu.Callsite[]
    callsites, _ = Cthulhu.find_callsites(interp, src, infos, mi, slottypes, optimize)
    @test all(c -> Cthulhu.get_effects(c) isa Cthulhu.Effects, callsites)
    return callsites
end

macro find_callsites_by_ftt(ex0...)
    return InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :find_callsites_by_ftt, ex0)
end
