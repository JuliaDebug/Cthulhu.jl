module test_terminal

using Test, REPL, Cthulhu, Revise
if isdefined(parentmodule(@__MODULE__), :VSCodeServer)
    using ..VSCodeServer
end

if !@isdefined(fake_terminal)
    include("FakeTerminals.jl")
    using .FakeTerminals
end

cread1(io) = readuntil(io, '↩'; keep=true)
cread(io) = cread1(io) * cread1(io)
strip_ansi_escape_sequences(str) = replace(str, r"\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])" => "")
function read_from(io)
    displayed = cread(io)
    text = strip_ansi_escape_sequences(displayed)
    return (displayed, text)
end

# For multi-call sites
module MultiCall
fmulti(::Int32) = 1
fmulti(::Float32) = rand(Float32)
fmulti(::Char) = 3
callfmulti(c) = fmulti(c[])
end

const keys = Dict(:up => "\e[A",
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
    @test Cthulhu.default_terminal() isa REPL.Terminals.TTYTerminal
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
            z = a * a
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
        terminal, input, output, err = FakeTerminals.fake_terminal()
        task = @async @with_try_stderr output descend(simplef, Tuple{Float32, Int32}; annotate_source=false, interruptexc=false, iswarn=false, terminal)

        displayed, text = read_from(output)
        @test occursin("simplef(a, b)", text)
        @test occursin(r"Base\.mul_float\(.*, .*\)::Float32", text)
        @test occursin('[' * colorize(true, 'o') * "]ptimize", displayed)
        @test occursin('[' * colorize(true, 'T') * "]yped", displayed)
        @test occursin("\nSelect a call to descend into", text) # beginning of the line
        @test occursin('•', text)
        write(input, 'o') # switch to unoptimized
        displayed, text = read_from(output)
        @test occursin("simplef(a, b)", text)
        @test occursin("::Const(*)", text)
        @test occursin("(z = (%1)(a, a))", text)
        @test occursin('[' * colorize(false, 'o') * "]ptimize", displayed)
        @test occursin("\nSelect a call to descend into", text) # beginning of the line
        @test occursin("• %2 = *(::Float32,::Float32)::Float32", text)

        # Call selection
        write(input, keys[:down])
        write(input, keys[:enter])
        cread1(output)
        displayed, text = read_from(output)
        @test occursin(r"• %\d = promote\(::Float32,::Int32\)::Tuple{Float32, Float32}", text)
        write(input, keys[:up])
        write(input, keys[:enter])
        cread1(output)
        displayed, text = read_from(output)
        @test occursin("• %2 = *(::Float32,::Float32)::Float32", text) # back to where we started
        write(input, 'o') # back to optimized
        displayed, text = read_from(output)
        @test !occursin("Variables", text)
        @test occursin(r"Base\.mul_float\(.*, .*\)::Float32", text)
        write(input, 'i') # show inline costs
        displayed, text = read_from(output)
        @test occursin(r"\d %\d = intrinsic Base\.mul_float", text)
        @test occursin('[' * colorize(true, 'i') * "]nlining costs", displayed)
        write(input, 'i') # hide inline costs
        displayed, text = read_from(output)
        write(input, 'o')
        displayed, text = read_from(output)
        @test occursin("Variables", text)
        write(input, 'w') # unoptimized + warntype
        displayed, text = read_from(output)
        @test occursin("Variables", text)
        @test occursin(r"z.*::Float32", text)
        @test occursin("Body", text)
        @test occursin('[' * colorize(true, 'w') * "]arn", displayed)
        @test occursin("\nSelect a call to descend into", text)
        @test occursin("• %2 = *(::Float32,::Float32)::Float32", text)

        # Source view
        write(input, 'S')
        displayed, text = read_from(output)
        @test occursin("z\e[36m::Float32\e[39m = (a\e[36m::Float32\e[39m * a\e[36m::Float32\e[39m)\e[36m::Float32\e[39m", displayed)
        @test occursin('[' * colorize(true, 'S') * "]ource", displayed)
        write(input, 's') # turn on syntax highlighting
        cread(output)
        write(input, 'S') # refresh source code
        displayed, text = read_from(output)
        @test occursin("simplef", text)
        @test occursin("\u001B", first(split(displayed, '\n')))
        @test occursin('[' * colorize(true, 's') * "]yntax", displayed)
        write(input, 's') # turn off syntax highlighting
        cread(output)  # off again
        # Back to typed code
        write(input, 'T'); cread(output)
        write(input, 'o')
        displayed, text = read_from(output)
        @test occursin('[' * colorize(true, 'T') * "]yped", displayed)

        # AST view
        write(input, 'A')
        displayed, text = read_from(output)
        @test occursin("Symbol simplef", text)
        @test occursin("Symbol call", text)
        @test occursin('[' * colorize(true, 'A') * "]ST", displayed)

        # LLVM view
        write(input, 'L')
        displayed, text = read_from(output)
        @test occursin(r"sitofp i(64|32)", text)
        @test occursin("fadd float", text)
        @test occursin("┌ @ promotion.jl", text) # debug info on by default
        @test occursin('[' * colorize(true, 'L') * "]LVM", displayed)
        # turn off debug info
        write(input, 'd'); cread(output)
        write(input, 'd'); cread(output)
        write(input, 'L')
        displayed, text = read_from(output)
        @test occursin('[' * colorize(false, 'd') * "]ebuginfo", displayed)
        @test !occursin("┌ @ promotion.jl", text)

        # Native code view
        write(input, 'N')
        displayed, text = read_from(output)
        @test occursin("retq", text)
        @test occursin('[' * colorize(true, 'N') * "]ative", displayed)
        # Typed view (by selector)
        write(input, 'T')
        displayed, text = read_from(output)
        @test occursin("Base.mul_float(a, a)::Float32", text)
        @test occursin('[' * colorize(true, 'T') * "]yped", displayed)

        # Parameter dumping
        write(input, 'P')
        displayed, text = read_from(output)

        # Revise
        # Use delays to ensure unambiguous differences in time stamps
        # (macOS is particularly sensitive) and execution of @async processes
        sleep(0.1)
        open(revisedfile, "w") do io
            println(io,
            """
            function simplef(a, b)
                z = a * b
                return z + b
            end
            """)
        end
        sleep(0.1)
        write(input, 'R')
        sleep(0.1)
        write(input, 'T'); cread(output)
        displayed, text = read_from(output)
        @test_broken occursin("z = a * b", displayed)
        write(input, 'q')
        wait(task)
        FakeTerminals.cleanup_fake_terminal(terminal, input, output, err)

        # Multicall & iswarn=true
        terminal, input, output, err = FakeTerminals.fake_terminal()
        task = @async @with_try_stderr output descend_code_warntype(MultiCall.callfmulti, Tuple{Any}; annotate_source=false, interruptexc=false, optimize=false, terminal)

        displayed, text = read_from(output)
        @test occursin("\nBody", text)
        @test occursin("\e[1m::Union{Float32, $Int}\e[22m\e[39m", displayed)
        @test occursin("Base.getindex(c)\e[91m\e[1m::Any\e[22m\e[39m", displayed)
        warncolor = if Cthulhu.is_expected_union(Union{Float32, Int64})
            Base.text_colors[Base.warn_color()]
        else
            Base.text_colors[Base.error_color()]
        end
        @test occursin("$(warncolor)%\e[39m3 = call → fmulti(::Any)::Union{Float32, Int64}", displayed)
        write(input, keys[:down])
        write(input, keys[:enter])
        displayed, text = read_from(output)
        @test occursin("%3 = fmulti(::Int32)::Union{Float32, $Int}", displayed)
        @test occursin("%3 = fmulti(::Float32)::Union{Float32, $Int}", displayed)
        @test occursin("%3 = fmulti(::Char)::Union{Float32, $Int}", displayed)
        write(input, 'q')
        wait(task)
        FakeTerminals.cleanup_fake_terminal(terminal, input, output, err)

        # Tasks (see the special handling in `_descend`)
        task_function() = @sync @async show(io, "Hello")
        terminal, input, output, err = FakeTerminals.fake_terminal()
        task = @async @with_try_stderr output @descend terminal=terminal annotate_source=false task_function()

        displayed, text = read_from(output)
        @test occursin(r"• %\d\d = task", text)
        write(input, keys[:enter])
        displayed, text = read_from(output)
        @test occursin("call show(::IO,::String)", text)
        write(input, 'q')
        wait(task)
        FakeTerminals.cleanup_fake_terminal(terminal, input, output, err)

        # descend with MethodInstances
        mi = Cthulhu.get_specialization(MultiCall.callfmulti, Tuple{typeof(Ref{Any}(1))})
        terminal, input, output, err = FakeTerminals.fake_terminal()
        task = @async @with_try_stderr output descend(mi; annotate_source=false, optimize=false, terminal)

        displayed, text = read_from(output)
        @test occursin("fmulti(::Any)", text)
        write(input, 'q')
        wait(task)
        FakeTerminals.cleanup_fake_terminal(terminal, input, output, err)

        terminal, input, output, err = FakeTerminals.fake_terminal()
        task = @async @with_try_stderr output descend_code_warntype(mi; annotate_source=false, interruptexc=false, optimize=false, terminal)

        displayed, text = read_from(output)
        @test occursin("Base.getindex(c)\e[91m\e[1m::Any\e[22m\e[39m", displayed)
        write(input, 'q')
        wait(task)
        FakeTerminals.cleanup_fake_terminal(terminal, input, output, err)

        # Fallback to typed code
        terminal, input, output, err = FakeTerminals.fake_terminal()
        task = @async @with_try_stderr output redirect_stderr(err) do
            descend(x -> [x], (Int,); annotate_source=true, interruptexc=false, optimize=false, terminal)
        end

        displayed, text = read_from(output)
        warnings = String(readavailable(err))
        @test occursin("\", Any} was not found", warnings)
        @test occursin("couldn't retrieve source", warnings)
        @test occursin("dynamic Base.vect(x)", text)
        @test occursin("(::$Int)::Vector{$Int}", text)
        write(input, 'q')
        wait(task)
        FakeTerminals.cleanup_fake_terminal(terminal, input, output, err)

        # `ascend`
        @noinline inner3(x) = 3x
        @inline   inner2(x) = 2*inner3(x)
                  inner1(x) = -1*inner2(x)
        inner1(0x0123)
        mi = Cthulhu.get_specialization(inner3, Tuple{UInt16})
        terminal, input, output, err = FakeTerminals.fake_terminal()
        task = @async @with_try_stderr output redirect_stderr(err) do
            ascend(terminal, mi)
        end

        write(input, keys[:down])
        write(input, keys[:enter])
        write(input, keys[:down])
        write(input, keys[:enter])
        displayed, text = read_from(output)
        lines = split(text, '\n')
        i = first(findfirst("inner3", lines[2]))
        @test first(findfirst("inner2", lines[3])) == i + 2
        from = findfirst(==("Open an editor at a possible caller of"), lines)
        @test isa(from, Int)
        @test occursin("inner2", lines[from + 3])
        write(input, 'q')
        write(input, 'q')
        wait(task)
        readavailable(err)
        FakeTerminals.cleanup_fake_terminal(terminal, input, output, err)

        # With backtraces
        bt = try sum([]); catch; catch_backtrace(); end
        terminal, input, output, err = FakeTerminals.fake_terminal()
        task = @async @with_try_stderr output ascend(terminal, bt)
        write(input, keys[:enter])
        displayed, text = read_from(output)
        @test occursin("Choose a call for analysis (q to quit):", text)
        @test occursin("mapreduce_empty", text)
        @test occursin("reduce_empty(op::Function, T::Type)", text)
        @test occursin("Select a call to descend into or ↩ to ascend.", text)
        write(input, 'q')
        write(input, 'q')
        wait(task)
        FakeTerminals.cleanup_fake_terminal(terminal, input, output, err)

        # With stacktraces
        st = try; sum([]); catch; stacktrace(catch_backtrace()); end
        terminal, input, output, err = FakeTerminals.fake_terminal()
        task = @async @with_try_stderr output ascend(terminal, st)
        write(input, keys[:enter])
        displayed, text = read_from(output)
        @test occursin("Choose a call for analysis (q to quit):", text)
        @test occursin("mapreduce_empty", text)
        @test occursin("reduce_empty(op::Function, T::Type)", text)
        @test occursin("Select a call to descend into or ↩ to ascend.", text)
        write(input, 'q')
        write(input, 'q')
        wait(task)
        FakeTerminals.cleanup_fake_terminal(terminal, input, output, err)

        # With ExceptionStack (e.g., REPL's global `err` variable)
        exception_stack = try; sum([]); catch e; Base.ExceptionStack([(exception=e, backtrace=stacktrace(catch_backtrace()))]); end
        terminal, input, output, err = FakeTerminals.fake_terminal()
        task = @async @with_try_stderr output ascend(terminal, exception_stack)
        write(input, keys[:enter])
        displayed, text = read_from(output)
        @test occursin("Choose a call for analysis (q to quit):", text)
        @test occursin("mapreduce_empty", text)
        @test occursin("reduce_empty(op::Function, T::Type)", text)
        @test occursin("Select a call to descend into or ↩ to ascend.", text)
        write(input, 'q')
        write(input, 'q')
        wait(task)
        FakeTerminals.cleanup_fake_terminal(terminal, input, output, err)
    finally
        # Restore the previous settings
        for fn in fieldnames(Cthulhu.CthulhuConfig)
            setfield!(Cthulhu.CONFIG, fn, getfield(CONFIG, fn))
        end
        rm(revisedfile)
    end
end

end # module test_terminal
