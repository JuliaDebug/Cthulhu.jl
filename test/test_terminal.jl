module test_terminal

using Test, REPL, Cthulhu, Revise
if isdefined(parentmodule(@__MODULE__), :VSCodeServer)
    using ..VSCodeServer
end

if !isdefined(@__MODULE__, :fake_terminal)
    @eval (@__MODULE__) begin
        includet(@__MODULE__, normpath(@__DIR__, "FakeTerminals.jl"))   # FIXME change to include
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

macro with_try_stderr(out, expr)
    quote
        try
            $(esc(expr))
        catch err
            bt = catch_backtrace()
            Base.display_error(stderr, err, bt)
            #close($(esc(out)))
        end
    end
end

@testset "Terminal" begin
    if isdefined(Base, :active_repl)
        @test Cthulhu.default_terminal() isa REPL.Terminals.TTYTerminal
    end
    colorize(use_color::Bool, c::Char) = Cthulhu.stringify() do io
        use_color ? printstyled(io, c; color=:cyan) : print(io, c)
    end
    # Write a file that we track with Revise. Creating it programmatically allows us to rewrite it with
    # different content
    revisedfile = tempname()
    open(revisedfile, "w") do io
        println(io,
        """
        function simplef(a, b)
            z = a*a
            return z + b
        end
        """)
    end
    includet(@__MODULE__, revisedfile)

    # Copy the user's current settings and set up the defaults
    CONFIG = deepcopy(Cthulhu.CONFIG)
    config = Cthulhu.CthulhuConfig()
    for fn in fieldnames(Cthulhu.CthulhuConfig)
        setfield!(Cthulhu.CONFIG, fn, getfield(config, fn))
    end

    try
        fake_terminal() do term, in, out, _
            t = @async begin
                @with_try_stderr out descend(simplef, Tuple{Float32, Int32}; annotate_source=false, interruptexc=false, terminal=term)
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
            @test occursin("• %1 = *(::Float32,::Float32)::Float32", lines)
            # Call selection
            write(in, keydict[:down])
            write(in, keydict[:enter])
            lines = cread1(out)
            lines = cread(out)
            @test occursin(r"• %\d = promote\(::Float32,::Int32\)::Tuple{Float32, Float32}", lines)
            write(in, keydict[:up])
            write(in, keydict[:enter])
            lines = cread1(out)
            lines = cread(out)
            @test occursin("• %1 = *(::Float32,::Float32)::Float32", lines)  # back to where we started
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
            @test occursin("• %1 = *(::Float32,::Float32)::Float32", lines)
            # Source view
            write(in, 'S')
            lines = cread(out)
            @test occursin("z\e[36m::Float32\e[39m = (a\e[36m::Float32\e[39m*a\e[36m::Float32\e[39m)\e[36m::Float32\e[39m", lines)
            @test occursin('[' * colorize(true, 'S') * "]ource", lines)
            # turn on syntax highlighting
            write(in, 's'); cread(out)
            write(in, 'S')
            lines = cread(out)
            @test occursin("simplef", lines)
            @test occursin("\u001B", first(split(lines, '\n')))
            @test occursin('[' * colorize(true, 's') * "]yntax", lines)
            write(in, 's'); cread(out)  # off again
            # Back to typed code
            write(in, 'T'); cread(out)
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
            # Use delays to ensure unambiguous differences in time stamps
            # (macOS is particularly sensitive) and execution of @async processes
            sleep(0.5)
            open(revisedfile, "w") do io
                println(io,
                """
                function simplef(a, b)
                    z = a*b
                    return z + b
                end
                """)
            end
            sleep(0.5)
            write(in, 'R')
            sleep(0.5)
            lines = cread(out)
            write(in, 'T')
            lines = cread(out)
            @test occursin("z = a * b", lines)
            write(in, 'q')
            wait(t)
        end
        # Multicall & iswarn=true
        fake_terminal() do term, in, out, _
            t = @async begin
                @with_try_stderr out descend_code_warntype(MultiCall.callfmulti, Tuple{Any}; annotate_source=false, interruptexc=false, optimize=false, terminal=term)
            end
            lines = cread(out)
            @test occursin("\nBody\e[", lines)
            @test occursin("\e[1m::Union{Float32, $Int}\e[22m\e[39m", lines)
            @test occursin("Base.getindex(c)\e[91m\e[1m::Any\e[22m\e[39m", lines)
            warncolor = if Cthulhu.is_expected_union(Union{Float32, Int64})
                Base.text_colors[Base.warn_color()]
            else
                Base.text_colors[Base.error_color()]
            end
            @test occursin("$(warncolor)%\e[39m2 = call → fmulti(::Any)::Union{Float32, Int64}", lines)
            if isdefined(Core.Compiler, :NoCallInfo)
                write(in, keydict[:down])
                write(in, keydict[:enter])
                lines = cread(out)
            else
                write(in, keydict[:enter])
                lines = cread1(out)
            end
            @test occursin("%2 = fmulti(::Int32)::Union{Float32, $Int}", lines)
            @test occursin("%2 = fmulti(::Float32)::Union{Float32, $Int}", lines)
            @test occursin("%2 = fmulti(::Char)::Union{Float32, $Int}", lines)
            write(in, 'q')
            wait(t)
        end
        # Tasks (see the special handling in `_descend`)
        ftask() = @sync @async show(io, "Hello")
        fake_terminal() do term, in, out, _
            t = @async begin
                @with_try_stderr out @descend terminal=term annotate_source=false ftask()
            end
            lines = cread(out)
            @test occursin(r"• %\d\d = task", lines)
            write(in, keydict[:enter])
            lines = cread(out)
            @test occursin("call show(::IO,::String)", lines)
            # TODO: Should this first `q` quite the session?
            write(in, 'q')
            write(in, 'q')
            wait(t)
        end
        # descend with MethodInstances
        mi = Cthulhu.get_specialization(MultiCall.callfmulti, Tuple{typeof(Ref{Any}(1))})
        fake_terminal() do term, in, out, _
            t = @async begin
                @with_try_stderr out descend(mi; annotate_source=false, interruptexc=false, optimize=false, terminal=term)
            end
            lines = cread(out)
            @test occursin("fmulti(::Any)", lines)
            write(in, 'q')
            wait(t)
        end
        fake_terminal() do term, in, out, _
            t = @async begin
                @with_try_stderr out descend_code_warntype(mi; annotate_source=false, interruptexc=false, optimize=false, terminal=term)
            end
            lines = cread(out)
            @test occursin("Base.getindex(c)\e[91m\e[1m::Any\e[22m\e[39m", lines)
            write(in, 'q')
            wait(t)
        end
        # Fallback to typed code
        fake_terminal() do term, in, out, err
            t = @async @with_try_stderr out begin
                redirect_stderr(err) do
                    descend((Int,); annotate_source=true, interruptexc=false, optimize=false, terminal=term) do x
                        [x]
                    end
                end
            end
            lines = cread1(out)
            wlines = readuntil(err, '\n')
            @test occursin("couldn't retrieve source", wlines)
            @test occursin("%1 = Base.vect(x)", lines)
            @test occursin("(::$Int)::Vector{$Int}", lines)
            write(in, 'q')
            readuntil(err, '\n')   # Clear out any extra output
            wait(t)
        end

        # ascend
        @noinline inner3(x) = 3x
        @inline   inner2(x) = 2*inner3(x)
                  inner1(x) = -1*inner2(x)
        inner1(0x0123)
        mi = Cthulhu.get_specialization(inner3, Tuple{UInt16})
        fake_terminal() do term, in, out, _
            t = @async begin
                @with_try_stderr out ascend(term, mi)
            end
            write(in, keydict[:down])
            write(in, keydict[:enter])
            write(in, keydict[:down])
            write(in, keydict[:enter])
            lines = split(cread(out), '\n')
            idx = first(findfirst("inner3", lines[2]))
            @test first(findfirst("inner2", lines[3])) == idx + 2
            ln = occursin(r"caller", lines[6]) ? 6 :
                 occursin(r"caller", lines[8]) ? 8 : error("not found")
            @test occursin("inner2", lines[ln+3])
            write(in, 'q')
            write(in, 'q')
            wait(t)
        end
        # With backtraces
        bt = try
            sum([])
        catch e
            catch_backtrace()
        end
        fake_terminal() do term, in, out, _
            t = @async begin
                @with_try_stderr out ascend(term, bt)
            end
            str = readuntil(out, 'v'; keep=true)
            @test occursin(r"zero.*Type{Any}", str)
            write(in, 'q')
            wait(t)
        end
        # With stacktraces
        st = try
            sum([])
        catch e
            stacktrace(catch_backtrace())
        end
        fake_terminal() do term, in, out, _
            t = @async begin
                @with_try_stderr out ascend(term, st)
            end
            str = readuntil(out, 'v'; keep=true)
            @test occursin(r"zero.*Type{Any}", str)
            write(in, 'q')
            wait(t)
        end
        # With ExceptionStack (e.g., REPL's global `err` variable)
        exstk = try
            sum([])
        catch e
            Base.ExceptionStack([(exception=e, backtrace=stacktrace(catch_backtrace()))])
        end
        fake_terminal() do term, in, out, _
            t = @async begin
                @with_try_stderr out ascend(term, exstk)
            end
            str = readuntil(out, 'v'; keep=true)
            @test occursin(r"zero.*Type{Any}", str)
            write(in, 'q')
            wait(t)
        end
    finally
        # Restore the previous settings
        for fn in fieldnames(Cthulhu.CthulhuConfig)
            setfield!(Cthulhu.CONFIG, fn, getfield(CONFIG, fn))
        end
        rm(revisedfile)
    end
end

end # module test_terminal
