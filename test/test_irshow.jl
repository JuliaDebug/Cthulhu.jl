module tests_irshow

using Cthulhu, Test, DeepDiffs

include("setup.jl")
include("irshowutils.jl")
include("IRShowSandbox.jl")

@testset "cthulhu_typed" begin
    tf = (true, false)

    @testset "optimize: $optimize" for optimize in tf
        (; src, infos, codeinst, rt, exct, effects, slottypes) = cthulhu_info(IRShowSandbox.foo, (Int, Int); optimize);

        @testset "debuginfo: $debuginfo" for debuginfo in instances(Cthulhu.DInfo.DebugInfo)
            @testset "iswarn: $iswarn" for iswarn in tf
                @testset "hide_type_stable: $hide_type_stable" for hide_type_stable in tf
                    @testset "inline_cost: $inline_cost" for inline_cost in tf
                        @testset "type_annotations: $type_annotations" for type_annotations in tf
                            !optimize && debuginfo === Cthulhu.DInfo.compact && continue
                            !optimize && inline_cost && continue

                            s = sprint(; context=:color=>true) do io
                                Cthulhu.cthulhu_typed(io, debuginfo,
                                                      src, rt, exct, effects, codeinst;
                                                      iswarn, hide_type_stable, inline_cost, type_annotations)
                            end
                            s = strip_base_linenums(s)

                            bname = irshow_filename("foo", optimize, debuginfo, iswarn, hide_type_stable, inline_cost, type_annotations)
                            fpath = normpath(@__DIR__, bname)

                            ground_truth = read(fpath, String)
                            if Sys.iswindows()
                                ground_truth = replace(ground_truth, "\r\n" => "\n")
                            end
                            @test s == ground_truth
                            s != ground_truth && println(deepdiff(s, ground_truth))
                        end
                    end
                end
            end
        end
    end
end

end # module tests_irshow
