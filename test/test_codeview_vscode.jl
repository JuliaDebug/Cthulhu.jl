module test_codeview_vscode
    using Cthulhu, Test, Revise, REPL, ..VSCodeServer
    
    @testset "VSCode descend test" begin
        if !isdefined(@__MODULE__, :fake_terminal)
            @eval (@__MODULE__) begin
                includet(@__MODULE__, normpath(@__DIR__, "FakeTerminals.jl"))   # FIXME change to include
                using .FakeTerminals
            end
        end

        function fib(n)
            if n <= 1 return 1 end
            return fib(n - 1) + fib(n - 2)
        end

        fake_terminal() do term, in, out, _
            @async @test_nowarn descend(fib, (Int,); terminal=term)
            write(in, 'q')
        end
    end
end