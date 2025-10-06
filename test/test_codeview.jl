module test_codeview

using Cthulhu, Test, Revise

include("setup.jl")

# NOTE setup for `cthulhu_ast`
include("TestCodeViewSandbox.jl")
using .TestCodeViewSandbox
Revise.track(TestCodeViewSandbox, normpath(@__DIR__, "TestCodeViewSandbox.jl"))

@testset "printer test" begin
    (; interp, src, infos, codeinst, rt, exct, effects, slottypes) = cthulhu_info(testf_revise);
    tf = (true, false)
    mi = codeinst.def
    @testset "codeview: $codeview" for codeview in Cthulhu.CODEVIEWS
        if !isdefined(@__MODULE__(), :Revise)
            codeview == Cthulhu.cthulhu_ast && continue
        end
        @testset "optimize: $optimize" for optimize in tf
            @testset "debuginfo: $debuginfo" for debuginfo in instances(Cthulhu.DInfo.DebugInfo)
                config = Cthulhu.CONFIG

                io = IOBuffer()
                src = Cthulhu.CC.typeinf_code(interp, mi, true)
                codeview(io, mi, src, optimize, debuginfo, Cthulhu.get_inference_world(interp), config)
                @test !isempty(String(take!(io))) # just check it works
            end
        end
    end

    @testset "debuginfo: $debuginfo" for debuginfo in instances(Cthulhu.DInfo.DebugInfo)
        @testset "iswarn: $iswarn" for iswarn in tf
            @testset "hide_type_stable: $hide_type_stable" for hide_type_stable in tf
                @testset "inline_cost: $inline_cost" for inline_cost in tf
                    @testset "type_annotations: $type_annotations" for type_annotations in tf
                        io = IOBuffer()
                        Cthulhu.cthulhu_typed(io, debuginfo,
                            src, rt, exct, effects, codeinst;
                            iswarn, hide_type_stable, inline_cost, type_annotations)
                        @test !isempty(String(take!(io))) # just check it works
                    end
                end
            end
        end
    end
end

@testset "hide type-stable statements" begin
    let # optimize code
        (; src, infos, codeinst, rt, exct, effects, slottypes) = @eval Module() begin
            const globalvar = Ref(42)
            $cthulhu_info() do
                a = sin(globalvar[])
                b = sin(undefvar)
                return (a, b)
            end
        end
        function prints(; kwargs...)
            io = IOBuffer()
            Cthulhu.cthulhu_typed(io, :none, src, rt, exct, effects, codeinst; kwargs...)
            return String(take!(io))
        end

        let # by default, should print every statement
            s = prints()
            @test occursin("globalvar", s)
            @test occursin("undefvar", s)
        end
        let # should omit type stable statements
            s = prints(; hide_type_stable=true)
            @test !occursin("globalvar", s)
            @test occursin("undefvar", s)
        end
    end

    let # unoptimize code
        (; src, infos, codeinst, rt, exct, effects, slottypes) = @eval Module() begin
            const globalvar = Ref(42)
            $cthulhu_info(; optimize=false) do
                a = sin(globalvar[])
                b = sin(undefvar)
                return (a, b)
            end
        end
        function prints(; kwargs...)
            io = IOBuffer()
            Cthulhu.cthulhu_typed(io, :none, src, rt, exct, effects, codeinst; kwargs...)
            return String(take!(io))
        end

        let # by default, should print every statement
            s = prints()
            @test occursin("globalvar", s)
            @test occursin("undefvar", s)
        end
        let # should omit type stable statements
            s = prints(; hide_type_stable=true)
            @test !occursin("globalvar", s)
            @test occursin("undefvar", s)
        end

        # should work for warn mode
        let
            s = prints(; iswarn=true)
            @test occursin("globalvar", s)
            @test occursin("undefvar", s)
        end
        let
            s = prints(; iswarn=true, hide_type_stable=true)
            @test !occursin("globalvar", s)
            @test occursin("undefvar", s)
        end
    end
end

@testset "Regressions" begin
    # Issue #675
    (; src, infos, codeinst, rt, exct, effects, slottypes) = cthulhu_info(NamedTuple; optimize=false)
    Cthulhu.cthulhu_typed(IOBuffer(), :none, src, rt, exct, effects, codeinst; annotate_source=true)
end

end # module test_codeview
