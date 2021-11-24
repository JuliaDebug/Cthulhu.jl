# NOTE setup too for `cthulhu_ast`
include("sandbox.jl")
using .CthulhuTestSandbox
Revise.track(CthulhuTestSandbox, normpath(@__DIR__, "sandbox.jl"))

@testset "printer test" begin
    interp, src, infos, mi, rt, slottypes = process(testf_revise);
    tf = (true, false)

    @testset "codeview: $codeview" for codeview in Cthulhu.CODEVIEWS
        if !@isdefined(Revise)
            codeview == Cthulhu.cthulhu_ast && continue
        end
        @testset "optimize: $optimize" for optimize in tf
            @testset "debuginfo: $debuginfo" for debuginfo in instances(Cthulhu.DInfo.DebugInfo)
                config = Cthulhu.CONFIG

                io = IOBuffer()
                codeview(io, mi, optimize, debuginfo, interp, config)
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
                            src, rt, mi;
                            iswarn, hide_type_stable, inline_cost, type_annotations)
                        @test !isempty(String(take!(io))) # just check it works
                    end
                end
            end
        end
    end
end

using PerformanceTestTools
PerformanceTestTools.@include("irshow.jl")

@testset "hide type-stable statements" begin
    let # optimize code
        _, src, infos, mi, rt, slottypes = @eval Module() begin
            const globalvar = Ref(42)
            $process() do
                a = sin(globalvar[])
                b = sin(undefvar)
                return (a, b)
            end
        end
        function prints(; kwargs...)
            io = IOBuffer()
            Cthulhu.cthulhu_typed(io, :none, src, rt, mi; kwargs...)
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
        _, src, infos, mi, rt, slottypes = @eval Module() begin
            const globalvar = Ref(42)
            $process(; optimize=false) do
                a = sin(globalvar[])
                b = sin(undefvar)
                return (a, b)
            end
        end
        function prints(; kwargs...)
            io = IOBuffer()
            Cthulhu.cthulhu_typed(io, :none, src, rt, mi; kwargs...)
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
