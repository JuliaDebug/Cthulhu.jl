using Test
using REPL
using Cthulhu
using Revise

if !isdefined(@__MODULE__, :fake_terminal)
    @eval begin
        includet(joinpath(@__DIR__, "FakeTerminals.jl"))   # FIXME change to include
        using .FakeTerminals
    end
end

cread1(io) = readuntil(io, '↩'; keep=true)
cread(io) = cread1(io) * cread1(io)

# For multi-call sites
module MultiCall
fmulti(::Int32) = 1
fmulti(::Float32) = rand(Float32)
fmulti(::Char) = 3
callfmulti(c) = fmulti(c[])
end

const keydict = Dict(:up => "\e[A",
                     :down => "\e[B",
                     :enter => '\r')

@testset "Terminal" begin
    @test Cthulhu.default_terminal() isa REPL.Terminals.TTYTerminal
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
                descend(simplef, Tuple{Float32, Int32}; interruptexc=false, terminal=term)
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
            # Call selection
            write(in, keydict[:down])
            write(in, keydict[:enter])
            lines = cread(out)
            lines = cread(out)
            @test occursin("• %1  = promote(::Float32,::Int32)::Tuple{Float32, Float32}", lines)
            write(in, keydict[:up])
            write(in, keydict[:enter])
            lines = cread(out)
            lines = cread(out)
            @test occursin("• %1  = *(::Float32,::Float32)::Float32", lines)  # back to where we started
            write(in, 'o')            # back to optimized
            lines = cread(out)
            @test !occursin("Variables", lines)
            @test occursin(r"Base\.mul_float\(.*, .*\)\u001B\[36m::Float32\u001B\[39m", lines)
            write(in, 'i')            # inline cost
            lines = cread(out)
            @test occursin(r"\u001B\[32m \d\u001B\[39m %\d += Base\.mul_float", lines)
            write(in, 'i')
            lines = cread(out)
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
            write(in, "q")
        end
        # Multicall & iswarn=true
        fake_terminal() do term, in, out
            @async begin
                descend_code_warntype(MultiCall.callfmulti, Tuple{Any}; interruptexc=false, optimize=false, terminal=term)
            end
            lines = cread(out)
            @test occursin("\nBody\e[91m\e[1m::Union{Float32, $Int}\e[22m\e[39m", lines)
            @test occursin("Base.getindex(c)\e[91m\e[1m::Any\e[22m\e[39m", lines)
            write(in, keydict[:enter])
            lines = cread1(out)
            @test occursin("%2  = fmulti(::Int32)::Union{Float32, $Int}", lines)
            @test occursin("%2  = fmulti(::Float32)::Union{Float32, $Int}", lines)
            @test occursin("%2  = fmulti(::Char)::Union{Float32, $Int}", lines)
            write(in, "q")
        end
        # Tasks (see the special handling in `_descend`)
        ftask() = @sync @async show(io, "Hello")
        fake_terminal() do term, in, out
            @async begin
                @descend terminal=term ftask()
            end
            lines = cread(out)
            @test occursin(r"• %\d\d  = task", lines)
            write(in, keydict[:enter])
            lines = cread(out)
            @test occursin("call show(::IO,::String)", lines)
            write(in, "q")
        end
        # ascend
        @noinline inner3(x) = 3x
        @inline   inner2(x) = 2*inner3(x)
                  inner1(x) = -1*inner2(x)
        inner1(0x0123)
        mi = Cthulhu.get_specialization(inner3, Tuple{UInt16})
        fake_terminal() do term, in, out
            @async begin
                ascend(term, mi)
            end
            write(in, keydict[:down])
            write(in, keydict[:enter])
            write(in, keydict[:down])
            write(in, keydict[:enter])
            lines = split(cread(out), '\n')
            idx = first(findfirst("inner3", lines[2]))
            @test first(findfirst("inner2", lines[3])) == idx + 2
            ln = occursin(r"caller.*inner3", lines[6]) ? 6 :
                 occursin(r"caller.*inner3", lines[8]) ? 8 : error("not found")
            @test occursin("inner2", lines[ln+1])
            @test any(str -> occursin("Variables", str), lines[ln+2:end])
            write(in, "q")
            write(in, "q")
        end
    finally
        # Restore the previous settings
        for fn in fieldnames(Cthulhu.CthulhuConfig)
            setfield!(Cthulhu.CONFIG, fn, getfield(CONFIG, fn))
        end
        rm(fn)
    end
end
