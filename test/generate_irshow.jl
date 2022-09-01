module generate_irshow

using Cthulhu

include("setup.jl")
include("irshowutils.jl")
include("IRShowSandbox.jl")

# to generate test cases for IRShow tests
function generate_test_cases(f, tt, fname=string(nameof(f)))
    outputs = Dict()
    tf = (true, false)
    for optimize in tf
        (; src, infos, mi, rt, effects, slottypes) = cthulhu_info(f, tt; optimize);
        for (debuginfo, iswarn, hide_type_stable, inline_cost, type_annotations) in Iterators.product(
            instances(Cthulhu.DInfo.DebugInfo), tf, tf, tf, tf,
        )
            !optimize && debuginfo === Cthulhu.DInfo.compact && continue
            !optimize && inline_cost && continue

            s = sprint(; context=:color=>true) do io
                Cthulhu.cthulhu_typed(io, debuginfo,
                                      src, rt, effects, mi;
                                      iswarn, hide_type_stable, inline_cost, type_annotations)
            end
            s = strip_base_linenums(s)

            bname = irshow_filename(fname, optimize, debuginfo, iswarn, hide_type_stable, inline_cost, type_annotations)
            fpath = normpath(@__DIR__, bname)

            write(fpath, s)

            in(s, values(outputs)) && (@show((optimize, debuginfo, iswarn, hide_type_stable, inline_cost, type_annotations)); @show(findfirst(==(s), pairs(outputs))))
            @assert !in(s, values(outputs))
            outputs[(optimize, debuginfo, iswarn, hide_type_stable, inline_cost, type_annotations)] = s
        end
    end
end

generate_test_cases(IRShowSandbox.foo, (Int,Int))

end # module generate_irshow
