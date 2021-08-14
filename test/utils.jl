function process(@nospecialize(f), @nospecialize(TT=()); optimize=true)
    (interp, mi) = Cthulhu.mkinterp(f, TT)
    (ci, rt, infos, slottypes) = Cthulhu.lookup(interp, mi, optimize; allow_no_codeinf=true)
    if ci !== nothing
        ci = Cthulhu.preprocess_ci!(ci, mi, optimize, Cthulhu.CthulhuConfig(dead_code_elimination=true))
    end
    interp, ci, infos, mi, rt, slottypes
end

function find_callsites_by_ftt(@nospecialize(f), @nospecialize(TT=Tuple{}); optimize=true)
    interp, ci, infos, mi, _, slottypes = process(f, TT; optimize)
    ci === nothing && return Cthulhu.Callsite[]
    callsites = Cthulhu.find_callsites(interp, ci, infos, mi, slottypes, optimize)
    return callsites
end

macro find_callsites_by_ftt(ex0...)
    return InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :find_callsites_by_ftt, ex0)
end

function strip_base_linenums(s)
    s = replace(s, r"((boot|int|refvalue|refpointer)\.jl:)\d+" => s"\1")
    return s
end

function irshow_filename(optimize, debuginfo, iswarn, hide_type_stable, inline_cost)
    return "test_output/foo-$(optimize ? :opt : :unopt)-$(debuginfo)-$(iswarn ? :warn : :nowarn)\
-$(hide_type_stable ? :hide_type_stable : :show_type_stable)-$(inline_cost ? :inline_cost : :nocost)"
end

# to generate test cases for IRShow tests
function generate_test_cases(foo)
    outputs = Dict()
    tf = (true, false)
    for optimize in tf
        _, src, infos, mi, rt, slottypes = process(foo, (Int, Int); optimize);
        for (debuginfo, iswarn, hide_type_stable, inline_cost) in Iterators.product(
            instances(Cthulhu.DInfo.DebugInfo), tf, tf, tf,
        )
            !optimize && debuginfo === Cthulhu.DInfo.compact && continue
            !optimize && inline_cost && continue

            s = sprint(; context=:color=>true) do io
                Cthulhu.cthulhu_typed(io, debuginfo,
                                      src, rt, mi;
                                      iswarn, hide_type_stable, inline_cost)
            end
            s = strip_base_linenums(s)
            write(irshow_filename(optimize, debuginfo, iswarn, hide_type_stable, inline_cost), s)

            in(s, values(outputs)) && (@show((optimize, debuginfo, iswarn, hide_type_stable, inline_cost)); @show(findfirst(==(s), pairs(outputs))))
            @assert !in(s, values(outputs))
            outputs[(optimize, debuginfo, iswarn, hide_type_stable, inline_cost)] = s
        end
    end
end
