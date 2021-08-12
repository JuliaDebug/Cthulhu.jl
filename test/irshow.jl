using Cthulhu, Test, DeepDiffs

include("utils.jl")

foo = Core.eval(Module(), Meta.parseall(raw"""
function foo(x, y)
    z = x + y
    if z < 4
        z += 1
    end
    u = (x -> x + z)(x)
    v = Ref{Union{Int, Missing}}(x)[] + y
    return z * 7
end
"""; filename="foobar.jl"))

@testset "IRShow tests" begin
    tf = (true, false)

    @testset "optimize: $optimize" for optimize in tf
        _, src, infos, mi, rt, slottypes = process(foo, (Int, Int); optimize);

        @testset "debuginfo: $debuginfo" for debuginfo in instances(Cthulhu.DInfo.DebugInfo)
            @testset "iswarn: $iswarn" for iswarn in tf
                @testset "hide_type_stable: $hide_type_stable" for hide_type_stable in tf
                    @testset "inline_cost: $inline_cost" for inline_cost in tf
                        !optimize && debuginfo === Cthulhu.DInfo.compact && continue
                        !optimize && inline_cost && continue

                        s = sprint(; context=:color=>true) do io
                            Cthulhu.cthulhu_typed(io, debuginfo,
                                                  src, rt, mi;
                                                  iswarn, hide_type_stable, inline_cost)
                        end
                        s = strip_base_linenums(s)

                        ground_truth = read(irshow_filename(optimize, debuginfo, iswarn, hide_type_stable, inline_cost), String)
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
