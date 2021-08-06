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
    if isdefined(Base, :active_repl)
        @test Cthulhu.default_terminal() isa REPL.Terminals.TTYTerminal
    end
    colorize(use_color::Bool, c::Char) = Cthulhu.stringify() do io
        use_color ? printstyled(io, c; color=:cyan) : print(io, c)
    end
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

    # Copy the user's current settings and set up the defaults
    CONFIG = deepcopy(Cthulhu.CONFIG)
    config = Cthulhu.CthulhuConfig()
    for fn in fieldnames(Cthulhu.CthulhuConfig)
        setfield!(Cthulhu.CONFIG, fn, getfield(config, fn))
    end

    try
        fake_terminal() do term, in, out
            @async begin
                descend(simplef, Tuple{Float32, Int32}; interruptexc=false, terminal=term)
            end
            lines = cread(out)
            @test occursin("invoke simplef(::Float32,::Int32)::Float32", lines)
            @test occursin(r"Base\.mul_float\(.*, .*\)\u001B\[36m::Float32\u001B\[39m", lines)
            @test occursin('[' * colorize(true, 'o') * "]ptimize", lines)
            @test occursin('[' * colorize(true, 'T') * "]yped", lines)

            @test occursin("\nSelect a call to descend into", lines)   # beginning of the line
            @test occursin('•', lines)
            write(in, 'o')            # switch to unoptimized
            lines = cread(out)
            @test occursin("invoke simplef(::Float32,::Int32)::Float32", lines)
            @test occursin(r"\(z = a \* a\)\u001B\[\d\dm::Float32\u001B\[39m", lines)
            @test occursin('[' * colorize(false, 'o') * "]ptimize", lines)
            @test occursin("\nSelect a call to descend into", lines)   # beginning of the line
            @test occursin("• %1  = *(::Float32,::Float32)::Float32", lines)
            # Call selection
            write(in, keydict[:down])
            write(in, keydict[:enter])
            lines = cread1(out)
            lines = cread(out)
            @test occursin("• %1  = promote(::Float32,::Int32)::Tuple{Float32, Float32}", lines)
            write(in, keydict[:up])
            write(in, keydict[:enter])
            lines = cread1(out)
            lines = cread(out)
            @test occursin("• %1  = *(::Float32,::Float32)::Float32", lines)  # back to where we started
            write(in, 'o')            # back to optimized
            lines = cread(out)
            @test !occursin("Variables", lines)
            @test occursin(r"Base\.mul_float\(.*, .*\)\u001B\[36m::Float32\u001B\[39m", lines)
            write(in, 'i')            # inline cost
            lines = cread(out)
            @test occursin(r"\u001B\[32m \d\u001B\[39m %\d += Base\.mul_float", lines)
            @test occursin('[' * colorize(true, 'i') * "]nlining costs", lines)
            write(in, 'i')
            lines = cread(out)
            write(in, 'o')
            lines = cread(out)
            @test occursin("Variables", lines)
            write(in, 'w')            # unoptimized + warntype
            lines = cread(out)
            @test occursin("Variables", lines)
            @test occursin(r"z.*::Float32", lines)
            @test occursin(r"\nBody.*Float32", lines)
            @test occursin('[' * colorize(true, 'w') * "]arn", lines)
            @test occursin("\nSelect a call to descend into", lines)   # beginning of the line
            @test occursin("• %1  = *(::Float32,::Float32)::Float32", lines)
            # Source view
            write(in, 'S')
            lines = cread(out)
            @test occursin("""
            \n\nfunction simplef(a, b)
                z = a*a
                return z + b
            end
            """, lines)
            @test occursin('[' * colorize(true, 'S') * "]ource", lines)
            # turn on syntax highlighting
            write(in, 's'); cread(out)
            write(in, 'S')
            lines = cread(out)
            @test occursin("simplef", lines)
            @test occursin("\u001B", first(split(lines, '\n')))
            @test occursin('[' * colorize(true, 's') * "]yntax", lines)
            write(in, 's'); cread(out)  # off again
            # Toggling 'o' goes back to typed code, make sure it also updates the selector status
            write(in, 'o')
            lines = cread(out)
            @test occursin('[' * colorize(true, 'T') * "]yped", lines)
            write(in, 'o'); cread(out)   # toggle it back for later tests
            # AST view
            write(in, 'A')
            lines = cread(out)
            @test occursin("Symbol simplef", lines)
            @test occursin("Symbol call", lines)
            @test occursin('[' * colorize(true, 'A') * "]ST", lines)
            # LLVM view
            write(in, 'L')
            lines = cread(out)
            @test occursin(r"sitofp i(64|32)", lines)
            @test occursin("fadd float", lines)
            @test occursin("┌ @ promotion.jl", lines)  # debug info on by default
            @test occursin('[' * colorize(true, 'L') * "]LVM", lines)
            # turn off debug info
            write(in, 'd'); cread(out)
            write(in, 'd'); cread(out)
            write(in, 'L')
            lines = cread(out)
            @test occursin('[' * colorize(false, 'd') * "]ebuginfo", lines)
            @test !occursin("┌ @ promotion.jl", lines)
            # Native-code view
            write(in, 'N')
            lines = cread(out)
            @test occursin(".text", lines) || occursin("__text", lines)
            @test occursin("retq", lines)
            @test occursin('[' * colorize(true, 'N') * "]ative", lines)
            # Typed-view (by selector)
            write(in, 'T')
            lines = cread(out)
            @test occursin(r"\(z \+ b\)\u001B\[\d\dm::Float32\u001B\[39m", lines)
            @test occursin('[' * colorize(true, 'T') * "]yped", lines)
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
            write(in, 'R')
            lines = cread(out)
            write(in, 'T')
            lines = cread(out)
            @test occursin("z = a * b", lines)
            write(in, 'q')
        end
        # Multicall & iswarn=true
        fake_terminal() do term, in, out
            @async begin
                descend_code_warntype(MultiCall.callfmulti, Tuple{Any}; interruptexc=false, optimize=false, terminal=term)
            end
            lines = cread(out)
            @test occursin("\nBody\e[91m\e[1m::Union{Float32, $Int}\e[22m\e[39m", lines)
            @test occursin("Base.getindex(c)\e[91m\e[1m::Any\e[22m\e[39m", lines)
            @test occursin("\e[31m%\e[39m2  = call #fmulti(::Any)::Union{Float32, Int64}", lines)
            write(in, keydict[:enter])
            lines = cread1(out)
            @test occursin("%2  = fmulti(::Int32)::Union{Float32, $Int}", lines)
            @test occursin("%2  = fmulti(::Float32)::Union{Float32, $Int}", lines)
            @test occursin("%2  = fmulti(::Char)::Union{Float32, $Int}", lines)
            write(in, 'q')
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
            write(in, 'q')
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
            write(in, 'q')
            write(in, 'q')
        end
    finally
        # Restore the previous settings
        for fn in fieldnames(Cthulhu.CthulhuConfig)
            setfield!(Cthulhu.CONFIG, fn, getfield(CONFIG, fn))
        end
        rm(fn)
    end
end
