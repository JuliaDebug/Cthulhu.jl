module test_codeview

using Test
using Logging: NullLogger, with_logger

using Cthulhu: Cthulhu, is_compiler_loaded, CthulhuState, view_function, CONFIG, set_config, cthulhu_typed

include("setup.jl")

if is_compiler_loaded()
    @eval using Revise
    Revise.track(Base) # get the `@info` log now, to avoid polluting test outputs later
    file = tempname() * ".jl"
    open(file, "w+") do io
        println(io, """
        module Sandbox

        function testf_revise()
            T = rand() > 0.5 ? Int64 : Float64
            sum(rand(T, 100))
        end

        end
        """)
    end
    include(file)
    (; testf_revise) = Sandbox
    Revise.track(Sandbox, file)
else
    function testf_revise()
        T = rand() > 0.5 ? Int64 : Float64
        sum(rand(T, 100))
    end
end

@testset "printer test" begin
    tf = (true, false)
    @testset "optimize: $optimize" for optimize in tf
        provider, mi, ci, result = cthulhu_info(testf_revise; optimize)
        @testset "view: $view" for view in (:source, :ast, :typed, :llvm, :native)
            view === :ast && !isdefined(@__MODULE__(), :Revise) && continue
            @testset "debuginfo: $debuginfo" for debuginfo in (:none, :source, :compact)
                config = set_config(CONFIG; view, debuginfo)
                state = CthulhuState(provider; config, ci, mi)
                io = IOBuffer()
                view_function(state)(io, provider, state, result)
                output = String(take!(io))
                @test !isempty(output) # just check it works
            end
        end
    end

    provider, mi, ci, result = cthulhu_info(testf_revise; optimize = true)
    @testset "debuginfo: $debuginfo" for debuginfo in (:none, :source, :compact)
        @testset "iswarn: $iswarn" for iswarn in tf
            @testset "hide_type_stable: $hide_type_stable" for hide_type_stable in tf
                @testset "inlining_costs: $inlining_costs" for inlining_costs in tf
                    @testset "type_annotations: $type_annotations" for type_annotations in tf
                        config = set_config(CONFIG; view = :typed, debuginfo, iswarn, hide_type_stable, inlining_costs, type_annotations)
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
            config = set_config(CONFIG; debuginfo = :none, kwargs...)
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
    provider, mi, ci, result = cthulhu_info(testf_revise; optimize=false)
    config = set_config(; view = :source, debuginfo = :none)
    state = CthulhuState(provider; config, ci, mi)
    io = IOBuffer()
    view_function(state)(io, provider, state, result)
    output = String(take!(io))
    @test isa(output, String)
end

end # module test_codeview
