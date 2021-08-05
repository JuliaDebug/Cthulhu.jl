# NOTE setup too for `cthulhu_ast`
include("sandbox.jl")
using .CthulhuTestSandbox
Revise.track(CthulhuTestSandbox, normpath(@__DIR__, "sandbox.jl"))

@testset "printer test" begin
    _, src, infos, mi, rt, slottypes = process(testf_revise);
    tf = (true, false)

    @testset "codeview: $codeview" for codeview in Cthulhu.CODEVIEWS
        if !@isdefined(Revise)
            codeview == Cthulhu.cthulhu_ast && continue
        end
        @testset "optimize: $optimize" for optimize in tf
            @testset "debuginfo: $debuginfo" for debuginfo in instances(Cthulhu.DInfo.DebugInfo)
                params = Cthulhu.current_params()
                config = Cthulhu.CONFIG

                io = IOBuffer()
                codeview(io, mi, optimize, debuginfo, params, config)
                @test !isempty(String(take!(io))) # just check it works
            end
        end
    end

    @testset "debuginfo: $debuginfo" for debuginfo in instances(Cthulhu.DInfo.DebugInfo)
        @testset "iswarn: $iswarn" for iswarn in tf
            @testset "hide_type_stable: $hide_type_stable" for hide_type_stable in tf
                @testset "inline_cost: $inline_cost" for inline_cost in tf
                    io = IOBuffer()
                    Cthulhu.cthulhu_typed(io, debuginfo,
                        src, rt, mi;
                        iswarn, hide_type_stable, inline_cost)
                    @test !isempty(String(take!(io))) # just check it works
                end
            end
        end
    end
end


foo = Core.eval(Module(), Meta.parseall(raw"""
function foo(x, y)
    z = x + y
    while z < 4
        z += 1
    end
    u = (x -> x + z)(x)
    v = Ref{Union{Int, Missing}}(x) + y
    return z * 7
end
"""; filename="foobar.jl"))

function strip_base_linenums(s)
    s = replace(s, r"((boot|int|refvalue|refpointer)\.jl:)\d+" => s"\1")
    return s
end

function filename(optimize, debuginfo, iswarn, hide_type_stable, inline_cost)
    return "test_output/foo-$(optimize ? :opt : :unopt)-$(debuginfo)-$(iswarn ? :warn : :nowarn)\
-$(hide_type_stable ? :hide_type_stable : :show_type_stable)-$(inline_cost ? :inline_cost : :nocost)"
end

#=
# to generate test cases
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
            write(filename(optimize, debuginfo, iswarn, hide_type_stable, inline_cost), s)

            in(s, values(outputs)) && (@show((optimize, debuginfo, iswarn, hide_type_stable, inline_cost)); @show(findfirst(==(s), pairs(outputs))))
            @assert !in(s, values(outputs))
            outputs[(optimize, debuginfo, iswarn, hide_type_stable, inline_cost)] = s
        end
    end
end

generate_test_cases(foo)
=#

using DeepDiffs

@testset "IRShow tests" begin
    tf = (true, false)

    @testset "optimize: $optimize" for optimize in tf
        _, src, infos, mi, rt, slottypes = process(foo, (Int, Int); optimize);

        # if coverage is enabled, src may contain `code_coverage_effect` expressions
        if optimize && Base.JLOptions().code_coverage != 0
            for bb in src.cfg.blocks
                for idx in bb.stmts
                    if src.stmts.inst[idx] == Expr(:code_coverage_effect)
                        src.stmts.inst[idx] = nothing
                    end
                end
            end
            src = Cthulhu.dce!(src, mi)
        end

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

                        ground_truth = read(filename(optimize, debuginfo, iswarn, hide_type_stable, inline_cost), String)
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
