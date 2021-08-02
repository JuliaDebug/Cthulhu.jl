using Test
using Cthulhu
using Revise

if !isdefined(@__MODULE__, :fake_terminal)
    @eval begin
        includet(joinpath(@__DIR__, "FakeTerminals.jl"))   # FIXME change to include
        using .FakeTerminals
    end
end

cread(io) = readuntil(io, '↩'; keep=true) * readuntil(io, '↩'; keep=true)

@testset "Terminal" begin
    # Write a file that we track with Revise. Creating it programmatically allows us to rewrite it with
    # different content
    fn = tempname()
    open(fn, "w") do io
        println(io,
        """
        function simplef(a, b)
            z = a*a
            return z + b
        end
        """)
    end
    includet(fn)

    CONFIG = deepcopy(Cthulhu.CONFIG)

    try
        fake_terminal() do term, in, out
            @async begin
                Cthulhu._descend(term, simplef, Tuple{Float32, Int32}; interruptexc=false, iswarn=false)
            end
            lines = cread(out)
            @test occursin("invoke simplef(::Float32,::Int32)::Float32", lines)
            @test occursin(r"Base\.mul_float\(.*, .*\)\u001B\[36m::Float32\u001B\[39m", lines)
            @test_broken occursin("\nSelect a call to descend into", lines)   # beginning of the line
            @test occursin('•', lines)
            write(in, 'o')            # switch to unoptimized
            lines = cread(out)
            @test occursin("invoke simplef(::Float32,::Int32)::Float32", lines)
            @test occursin(r"\(z = a \* a\)\u001B\[\d\dm::Float32\u001B\[39m", lines)
            @test_broken occursin("\nSelect a call to descend into", lines)   # beginning of the line
            @test occursin("• %1  = *(::Float32,::Float32)::Float32", lines)
            write(in, 'o')            # back to optimized
            lines = cread(out)
            @test !occursin("Variables", lines)
            @test occursin(r"Base\.mul_float\(.*, .*\)\u001B\[36m::Float32\u001B\[39m", lines)
            write(in, 'o')
            lines = cread(out)
            @test !occursin("Variables", lines)
            write(in, 'w')            # unoptimized + warntype
            lines = cread(out)
            @test occursin("Variables", lines)
            @test occursin(r"z.*::Float32", lines)
            @test occursin(r"\nBody.*Float32", lines)
            @test_broken occursin("\nSelect a call to descend into", lines)   # beginning of the line
            @test occursin("• %1  = *(::Float32,::Float32)::Float32", lines)
            # turn off syntax highlighting
            write(in, "s"); cread(out)
            write(in, "S")
            lines = cread(out)
            @test occursin("""
            \n\nfunction simplef(a, b)
                z = a*a
                return z + b
            end
            """, lines)
            write(in, "A")
            lines = cread(out)
            @test occursin("Symbol simplef", lines)
            @test occursin("Symbol call", lines)
            write(in, "L")
            lines = cread(out)
            @test occursin(r"sitofp i(64|32)", lines)
            @test occursin("fadd float", lines)
            @test occursin("┌ @ promotion.jl", lines)  # debug info on by default
            # turn off debug info
            write(in, "d"); cread(out)
            write(in, "d"); cread(out)
            write(in, "L")
            lines = cread(out)
            @test !occursin("┌ @ promotion.jl", lines)
            write(in, "N")
            lines = cread(out)
            @test occursin(".text", lines) || occursin("__text", lines)
            @test occursin("retq", lines)
            # Revise
            open(fn, "w") do io
                println(io,
                """
                function simplef(a, b)
                    z = a*b
                    return z + b
                end
                """)
            end
            sleep(0.1)
            write(in, "R")
            lines = cread(out)
            write(in, "o"); cread(out)     # optimized code
            write(in, "o")     # unoptimized code
            lines = cread(out)
            @test occursin("z = a * b", lines)
        end
    finally
        # Restore the previous settings
        for fn in fieldnames(Cthulhu.CthulhuConfig)
            setfield!(Cthulhu.CONFIG, fn, getfield(CONFIG, fn))
        end
        rm(fn)
    end
end
