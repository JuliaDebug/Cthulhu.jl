module test_codeview

using Test, Revise, Accessors
using Logging: NullLogger, with_logger

import Cthulhu as _Cthulhu
const Cthulhu = _Cthulhu.CTHULHU_MODULE[]
using .Cthulhu: CthulhuState, view_function, CONFIG, cthulhu_typed

include("setup.jl")

# NOTE setup for `cthulhu_ast`
include("TestCodeViewSandbox.jl")
(; testf_revise) = TestCodeViewSandbox
Revise.track(TestCodeViewSandbox, normpath(@__DIR__, "TestCodeViewSandbox.jl"))

@testset "printer test" begin
    tf = (true, false)
    @testset "optimize: $optimize" for optimize in tf
        provider, mi, ci, result = cthulhu_info(testf_revise; optimize)
        @testset "view: $view" for view in (:source, :ast, :typed, :llvm, :native)
            view === :ast && !isdefined(@__MODULE__(), :Revise) && continue
            @testset "debuginfo: $debuginfo" for debuginfo in (:none, :source, :compact)
                config = setproperties(CONFIG, (; view, debuginfo))
                state = CthulhuState(provider; config, ci, mi)
                io = IOBuffer()
                view_function(state)(io, provider, state, result)
                @test !isempty(String(take!(io))) # just check it works
            end
        end
    end

    provider, mi, ci, result = cthulhu_info(testf_revise; optimize = true)
    @testset "debuginfo: $debuginfo" for debuginfo in (:none, :source, :compact)
        @testset "iswarn: $iswarn" for iswarn in tf
            @testset "hide_type_stable: $hide_type_stable" for hide_type_stable in tf
                @testset "inlining_costs: $inlining_costs" for inlining_costs in tf
                    @testset "type_annotations: $type_annotations" for type_annotations in tf
                        config = setproperties(CONFIG, (; view = :typed, debuginfo, iswarn, hide_type_stable, inlining_costs, type_annotations))
                        state = CthulhuState(provider; config, ci, mi)
                        io = IOBuffer()
                        view_function(state)(io, provider, state, result)
                        @test !isempty(String(take!(io))) # just check it works
                    end
                end
            end
        end
    end
end

@testset "hide type-stable statements" begin
    function printer(provider, mi, ci, result)
        return function prints(; kwargs...)
            io = IOBuffer()
            config = setproperties(CONFIG, (; debuginfo = :none, kwargs...))
            state = CthulhuState(provider; config, ci, mi)
            with_logger(NullLogger()) do
                cthulhu_typed(io, provider, state, result)
            end
            return String(take!(io))
        end
    end

    @testset "optimized" begin
        provider, mi, ci, result = @eval Module() begin
            const globalvar = Ref(42)
            $cthulhu_info() do
                a = sin(globalvar[])
                b = sin(undefvar)
                return (a, b)
            end
        end
        prints = printer(provider, mi, ci, result)

        # by default, should print every statement
        s = prints()
        @test occursin("globalvar", s)
        @test occursin("undefvar", s)

        # should omit type stable statements
        s = prints(; hide_type_stable=true)
        @test !occursin("globalvar", s)
        @test occursin("undefvar", s)
    end

    @testset "unoptimized" begin
        provider, mi, ci, result = @eval Module() begin
            const globalvar = Ref(42)
            $cthulhu_info(; optimize=false) do
                a = sin(globalvar[])
                b = sin(undefvar)
                return (a, b)
            end
        end
        prints = printer(provider, mi, ci, result)

        # by default, should print every statement
        s = prints()
        @test occursin("globalvar", s)
        @test occursin("undefvar", s)

        # should omit type stable statements
        s = prints(; hide_type_stable=true)
        @test !occursin("globalvar", s)
        @test occursin("undefvar", s)

        # should work for warn mode
        s = prints(; iswarn=true)
        @test occursin("globalvar", s)
        @test occursin("undefvar", s)
        s = prints(; iswarn=true, hide_type_stable=true)
        @test !occursin("globalvar", s)
        @test occursin("undefvar", s)
    end
end;

@testset "Regressions" begin
    # Issue #675
    (; src, infos, codeinst, rt, exct, effects, slottypes) = cthulhu_info(NamedTuple; optimize=false)
    Cthulhu.cthulhu_typed(IOBuffer(), :none, src, rt, exct, effects, codeinst; annotate_source=true)
end

end # module test_codeview
