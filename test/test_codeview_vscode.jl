module test_codeview_vscode

using Test, Revise, REPL, ..VSCodeServer, TypedSyntax
import TypedSyntax: InlayHintKinds

include("cthulhu.jl")
include("test_vscode_example_functions.jl")

@testset "VSCode descend test" begin
    if !isdefined(@__MODULE__, :fake_terminal)
        @eval (@__MODULE__) begin
            includet(@__MODULE__, normpath(@__DIR__, "FakeTerminals.jl"))   # FIXME change to include
            using .FakeTerminals
        end
    end

    function equal_upto_ordering(x, y)
        if length(x) != length(y)
            return false
        end

        while length(x) > 0
            element = pop!(x)
            found_idx = findfirst(x->x==element, y)
            if !isnothing(found_idx)
                deleteat!(y, found_idx)
            else
                return false
            end
        end
        return true
    end

    for inlay_types_vscode in (true, false), diagnostics_vscode in (true, false), iswarn in (true, false), hide_type_stable in (true, false)
        @testset "fib inlay_types_vscode=$inlay_types_vscode, diagnostics_vscode=$diagnostics_vscode, iswarn=$iswarn, hide_type_stable=$hide_type_stable" begin
            VSCodeServer.reset_test_containers()

            fake_terminal() do term, in, out, _
                t = @async begin
                    @test_nowarn descend(fib, (Int,); terminal=term, iswarn, hide_type_stable, inlay_types_vscode, diagnostics_vscode)
                end
                write(in, 'q')
                wait(t)
            end

            if inlay_types_vscode
                @test length(VSCodeServer.inlay_hints) == 2
                @test isempty(VSCodeServer.inlay_hints[2])
            else
                @test length(VSCodeServer.inlay_hints) == 1
                @test isempty(VSCodeServer.inlay_hints[1])
            end

            if diagnostics_vscode
                @test length(VSCodeServer.diagnostics) == 2
                @test isempty(VSCodeServer.diagnostics[2])
                if !iswarn
                    @test isempty(VSCodeServer.diagnostics[1])
                end
            end

            if !hide_type_stable && inlay_types_vscode
                @test equal_upto_ordering(first(values(VSCodeServer.inlay_hints[1])), [
                    TypedSyntax.InlayHint(1, 14, "::$Int", 1)
                    TypedSyntax.InlayHint(1, 15, "::$Int", 1)
                    TypedSyntax.InlayHint(3, 11, "(", 1)
                    TypedSyntax.InlayHint(3, 15, "(", 1)
                    TypedSyntax.InlayHint(3, 16, "::$Int", 1)
                    TypedSyntax.InlayHint(3, 20, ")::$Int", 1)
                    TypedSyntax.InlayHint(3, 21, "::$Int", 1)
                    TypedSyntax.InlayHint(3, 28, "(", 1)
                    TypedSyntax.InlayHint(3, 29, "::$Int", 1)
                    TypedSyntax.InlayHint(3, 33, ")::$Int", 1)
                    TypedSyntax.InlayHint(3, 34, "::$Int", 1)
                    TypedSyntax.InlayHint(3, 34, ")::$Int", 1)
                ])
            elseif hide_type_stable && inlay_types_vscode
                @test isempty(VSCodeServer.inlay_hints[1])
            end
        end
    end

    for inlay_types_vscode in (true, false), diagnostics_vscode in (true, false), iswarn in (true, false), hide_type_stable in (true, false)
        @testset "fVSCode inlay_types_vscode=$inlay_types_vscode, diagnostics_vscode=$diagnostics_vscode, iswarn=$iswarn, hide_type_stable=$hide_type_stable" begin
            VSCodeServer.reset_test_containers()

            fake_terminal() do term, in, out, _
                t = @async begin
                    @test_nowarn descend(fVSCode, (Int,); terminal=term, iswarn, hide_type_stable, inlay_types_vscode, diagnostics_vscode)
                end
                write(in, 'q')
                wait(t)
            end

            if inlay_types_vscode
                @test length(VSCodeServer.inlay_hints) == 2
                @test isempty(VSCodeServer.inlay_hints[2])
            else
                @test length(VSCodeServer.inlay_hints) == 1
                @test isempty(VSCodeServer.inlay_hints[1])
            end

            if diagnostics_vscode
                @test length(VSCodeServer.diagnostics) == 2
                @test isempty(VSCodeServer.diagnostics[2])
                if iswarn
                    @test equal_upto_ordering(getproperty.(VSCodeServer.diagnostics[1], :line), [8, 11])
                    @test getproperty.(VSCodeServer.diagnostics[1], :severity) == [TypedSyntax.DiagnosticKinds.Information, TypedSyntax.DiagnosticKinds.Information]
                    @test getproperty.(VSCodeServer.diagnostics[1], :msg) == ["Unstable Type", "Unstable Type"]
                else
                    @test isempty(VSCodeServer.diagnostics[1])
                end
            end

            if !hide_type_stable && inlay_types_vscode
                if iswarn
                    @test equal_upto_ordering(first(values(VSCodeServer.inlay_hints[1])), [
                        TypedSyntax.InlayHint(7, 18, "::$Int", 1)
                        TypedSyntax.InlayHint(7, 19, "::Union{Float64, $Int}", nothing)
                        TypedSyntax.InlayHint(8, 5, "::$Int", 1)
                        TypedSyntax.InlayHint(8, 8, "(", 1)
                        TypedSyntax.InlayHint(8, 9, "::$Int", 1)
                        TypedSyntax.InlayHint(8, 13, ")::$Int", 1)
                        TypedSyntax.InlayHint(9, 5, "::$Int", 1)
                        TypedSyntax.InlayHint(9, 8, "(", 1)
                        TypedSyntax.InlayHint(9, 13, "::$Int", 1)
                        TypedSyntax.InlayHint(9, 13, ")::$Int", 1)
                        TypedSyntax.InlayHint(10, 11, "(", 1)
                        TypedSyntax.InlayHint(10, 12, "::$Int", 1)
                        TypedSyntax.InlayHint(10, 16, "(", 1)
                        TypedSyntax.InlayHint(10, 17, "::$Int", 1)
                        TypedSyntax.InlayHint(10, 21, ")::Bool", 1)
                        TypedSyntax.InlayHint(10, 26, "::$Int", 1)
                        TypedSyntax.InlayHint(10, 32, "::Float64", 1)
                        TypedSyntax.InlayHint(10, 33, ")::Union{Float64, $Int}", nothing)
                    ])

                else
                    @test equal_upto_ordering(first(values(VSCodeServer.inlay_hints[1])), [
                        TypedSyntax.InlayHint(7, 18, "::$Int", 1)
                        TypedSyntax.InlayHint(7, 19, "::Union{Float64, $Int}", 1)
                        TypedSyntax.InlayHint(8, 5, "::$Int", 1)
                        TypedSyntax.InlayHint(8, 8, "(", 1)
                        TypedSyntax.InlayHint(8, 9, "::$Int", 1)
                        TypedSyntax.InlayHint(8, 13, ")::$Int", 1)
                        TypedSyntax.InlayHint(9, 5, "::$Int", 1)
                        TypedSyntax.InlayHint(9, 8, "(", 1)
                        TypedSyntax.InlayHint(9, 13, "::$Int", 1)
                        TypedSyntax.InlayHint(9, 13, ")::$Int", 1)
                        TypedSyntax.InlayHint(10, 11, "(", 1)
                        TypedSyntax.InlayHint(10, 12, "::$Int", 1)
                        TypedSyntax.InlayHint(10, 16, "(", 1)
                        TypedSyntax.InlayHint(10, 17, "::$Int", 1)
                        TypedSyntax.InlayHint(10, 21, ")::Bool", 1)
                        TypedSyntax.InlayHint(10, 26, "::$Int", 1)
                        TypedSyntax.InlayHint(10, 32, "::Float64", 1)
                        TypedSyntax.InlayHint(10, 33, ")::Union{Float64, $Int}", 1)
                    ])
                end
            elseif hide_type_stable && inlay_types_vscode
                if iswarn
                    @test equal_upto_ordering(first(values(VSCodeServer.inlay_hints[1])), [
                        TypedSyntax.InlayHint(7, 19, "::Union{Float64, $Int}", nothing)
                        TypedSyntax.InlayHint(10, 11, "(", 1)
                        TypedSyntax.InlayHint(10, 33, ")::Union{Float64, $Int}", nothing)
                    ])
                else
                    @test equal_upto_ordering(first(values(VSCodeServer.inlay_hints[1])), [
                        TypedSyntax.InlayHint(7, 19, "::Union{Float64, $Int}", 1)
                        TypedSyntax.InlayHint(10, 11, "(", 1)
                        TypedSyntax.InlayHint(10, 33, ")::Union{Float64, $Int}", 1)
                    ])
                end
            end
        end
    end

    for inlay_types_vscode in (true, false), diagnostics_vscode in (true, false), iswarn in (true, false), hide_type_stable in (true, false)
        @testset "fibcall Float64 inlay_types_vscode=$inlay_types_vscode, diagnostics_vscode=$diagnostics_vscode, iswarn=$iswarn, hide_type_stable=$hide_type_stable" begin
            VSCodeServer.reset_test_containers()

            fake_terminal() do term, in, out, _
                t = @async begin
                    @test_nowarn descend(fibcall, (Float64,); terminal=term, iswarn, hide_type_stable, inlay_types_vscode, diagnostics_vscode)
                end
                write(in, 'q')
                wait(t)
            end

            if inlay_types_vscode
                @test length(VSCodeServer.inlay_hints) == 2
                @test isempty(VSCodeServer.inlay_hints[2])
            else
                @test length(VSCodeServer.inlay_hints) == 1
                @test isempty(VSCodeServer.inlay_hints[1])
            end

            if inlay_types_vscode || (diagnostics_vscode && iswarn)
                @test length(VSCodeServer.diagnostics) == 2
                @test isempty(VSCodeServer.diagnostics[2])
                @test length(VSCodeServer.diagnostics[1]) == 1
                @test VSCodeServer.diagnostics[1][1].severity == TypedSyntax.DiagnosticKinds.Information
                @test VSCodeServer.diagnostics[1][1].line == 2
                @test VSCodeServer.diagnostics[1][1].msg == "Cthulhu disabled: This function was called multiple times with different argument types"
            end

            if !hide_type_stable && inlay_types_vscode
                @test equal_upto_ordering(first(values(VSCodeServer.inlay_hints[1])), [
                    TypedSyntax.InlayHint(14, 18, "::Float64", 1)
                    TypedSyntax.InlayHint(14, 19, "::$Int", 1)
                    TypedSyntax.InlayHint(15, 11, "::Type{$Int}", 1)
                    TypedSyntax.InlayHint(15, 13, "::Float64", 1)
                    TypedSyntax.InlayHint(15, 14, "::$Int", 1)
                    TypedSyntax.InlayHint(16, 9, "::Float64", 1)
                    TypedSyntax.InlayHint(16, 10, "::$Int", 1)
                ]) || equal_upto_ordering(first(values(VSCodeServer.inlay_hints[1])), [
                    TypedSyntax.InlayHint(14, 18, "::Float64", 1)
                    TypedSyntax.InlayHint(14, 19, "::$Int", 1)
                    TypedSyntax.InlayHint(15, 13, "::Float64", 1)
                    TypedSyntax.InlayHint(15, 14, "::$Int", 1)
                    TypedSyntax.InlayHint(16, 9, "::Float64", 1)
                    TypedSyntax.InlayHint(16, 10, "::$Int", 1)
                ]) # Workaround for #530
            elseif hide_type_stable && inlay_types_vscode
                @test isempty(VSCodeServer.inlay_hints[1])
            end
        end
    end

    for inlay_types_vscode in (true, false), diagnostics_vscode in (true, false), iswarn in (true, false), hide_type_stable in (true, false)
        @testset "fibcall Int inlay_types_vscode=$inlay_types_vscode, diagnostics_vscode=$diagnostics_vscode, iswarn=$iswarn, hide_type_stable=$hide_type_stable" begin
            VSCodeServer.reset_test_containers()

            fake_terminal() do term, in, out, _
                t = @async begin
                    @test_nowarn descend(fibcall, (Int,); terminal=term, iswarn, hide_type_stable, inlay_types_vscode, diagnostics_vscode)
                end
                write(in, 'q')
                wait(t)
            end

            if inlay_types_vscode
                @test length(VSCodeServer.inlay_hints) == 2
                @test isempty(VSCodeServer.inlay_hints[2])
            else
                @test length(VSCodeServer.inlay_hints) == 1
                @test isempty(VSCodeServer.inlay_hints[1])
            end

            if diagnostics_vscode
                @test length(VSCodeServer.diagnostics) == 2
                @test isempty(VSCodeServer.diagnostics[2])
                if !iswarn
                    @test isempty(VSCodeServer.diagnostics[1])
                end
            end

            if !hide_type_stable && inlay_types_vscode
                @test equal_upto_ordering(first(values(VSCodeServer.inlay_hints[1])), [
                    TypedSyntax.InlayHint(14, 18, "::$Int", 1)
                    TypedSyntax.InlayHint(14, 19, "::$Int", 1)
                    TypedSyntax.InlayHint(15, 11, "::Type{$Int}", 1)
                    TypedSyntax.InlayHint(15, 13, "::$Int", 1)
                    TypedSyntax.InlayHint(15, 14, "::$Int", 1)
                    TypedSyntax.InlayHint(16, 9, "::$Int", 1)
                    TypedSyntax.InlayHint(16, 10, "::$Int", 1)
                    TypedSyntax.InlayHint(1, 14, "::$Int", 1)
                    TypedSyntax.InlayHint(1, 15, "::$Int", 1)
                    TypedSyntax.InlayHint(3, 11, "(", 1)
                    TypedSyntax.InlayHint(3, 15, "(", 1)
                    TypedSyntax.InlayHint(3, 16, "::$Int", 1)
                    TypedSyntax.InlayHint(3, 20, ")::$Int", 1)
                    TypedSyntax.InlayHint(3, 21, "::$Int", 1)
                    TypedSyntax.InlayHint(3, 28, "(", 1)
                    TypedSyntax.InlayHint(3, 29, "::$Int", 1)
                    TypedSyntax.InlayHint(3, 33, ")::$Int", 1)
                    TypedSyntax.InlayHint(3, 34, "::$Int", 1)
                    TypedSyntax.InlayHint(3, 34, ")::$Int", 1)
                ]) || equal_upto_ordering(first(values(VSCodeServer.inlay_hints[1])), [
                    TypedSyntax.InlayHint(14, 18, "::$Int", 1)
                    TypedSyntax.InlayHint(14, 19, "::$Int", 1)
                    TypedSyntax.InlayHint(15, 13, "::$Int", 1)
                    TypedSyntax.InlayHint(15, 14, "::$Int", 1)
                    TypedSyntax.InlayHint(16, 9, "::$Int", 1)
                    TypedSyntax.InlayHint(16, 10, "::$Int", 1)
                    TypedSyntax.InlayHint(1, 14, "::$Int", 1)
                    TypedSyntax.InlayHint(1, 15, "::$Int", 1)
                    TypedSyntax.InlayHint(3, 11, "(", 1)
                    TypedSyntax.InlayHint(3, 15, "(", 1)
                    TypedSyntax.InlayHint(3, 16, "::$Int", 1)
                    TypedSyntax.InlayHint(3, 20, ")::$Int", 1)
                    TypedSyntax.InlayHint(3, 21, "::$Int", 1)
                    TypedSyntax.InlayHint(3, 28, "(", 1)
                    TypedSyntax.InlayHint(3, 29, "::$Int", 1)
                    TypedSyntax.InlayHint(3, 33, ")::$Int", 1)
                    TypedSyntax.InlayHint(3, 34, "::$Int", 1)
                    TypedSyntax.InlayHint(3, 34, ")::$Int", 1)
                ]) # Workaround for #530
            elseif hide_type_stable && inlay_types_vscode
                @test isempty(VSCodeServer.inlay_hints[1])
            end
        end
    end
end

end
