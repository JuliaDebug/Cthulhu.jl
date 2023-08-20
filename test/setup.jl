using Test, Cthulhu, InteractiveUtils
if isdefined(parentmodule(@__MODULE__), :VSCodeServer)
    using ..VSCodeServer
end

function cthulhu_info(@nospecialize(f), @nospecialize(TT=()); optimize=true)
    (interp, mi) = Cthulhu.mkinterp(Core.Compiler.NativeInterpreter(), f, TT)
    (; src, rt, infos, slottypes, effects) = Cthulhu.lookup(interp, mi, optimize; allow_no_src=true)
    if src !== nothing
        src = Cthulhu.preprocess_ci!(src, mi, optimize, Cthulhu.CthulhuConfig(dead_code_elimination=true))
    end
    (; interp, src, infos, mi, rt, slottypes, effects)
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

@static if isdefined(Core.Compiler, :is_foldable_nothrow)
    using Core.Compiler: is_foldable_nothrow
else
    const is_foldable_nothrow = Core.Compiler.is_total
end
