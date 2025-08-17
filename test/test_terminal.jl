# module test_terminal

using Core: Const
using Test, REPL, Cthulhu, Revise
using Cthulhu.Testing: FakeTerminal
global _Cthulhu::Module = Cthulhu.CTHULHU_MODULE[]

if isdefined(parentmodule(@__MODULE__), :VSCodeServer)
    using ..VSCodeServer
end

# We use the '↩' character to know when to stop reading, otherwise
# the read will block indefinitely. Note that we do rely on what Cthulhu
# prints, and because we display it twice (once during instructions, once
# at the end), we have to use `cread1` twice.
cread1(terminal) = readuntil(terminal.output, '↩'; keep=true)
cread(terminal) = cread1(terminal) * cread1(terminal)
strip_ansi_escape_sequences(str) = replace(str, r"\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])" => "")
function read_from(terminal)
    displayed = cread(terminal)
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

function wait_for(task::Task, timeout = 10.0)
    t0 = time()
    @goto body
    while time() - t0 < timeout
        @label body
        istaskfailed(task) && return wait(task)
        istaskdone(task) && return true
        yield()
    end
    return false
end

function end_terminal_session(terminal, task)
    readavailable(terminal.output)
    if !wait_for(task, 0)
        write(terminal, 'q')
        readavailable(terminal.output)
        @assert wait_for(task)
    end
    finalize(terminal)
    return istaskdone(task)
end

@testset "Terminal" begin
    @test _Cthulhu.default_terminal() isa REPL.Terminals.TTYTerminal
    colorize(active_option::Bool, c::Char) = _Cthulhu.stringify() do io
        active_option ? printstyled(io, c; bold=true, color=:green) : printstyled(io, c; color=:red)
    end

    colorize(s::AbstractString; color::Symbol = :cyan) = _Cthulhu.stringify() do io
        printstyled(io, s; color)
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

    _Cthulhu.CONFIG = _Cthulhu.CthulhuConfig()

    terminal = FakeTerminal()
    task = @async @with_try_stderr terminal.output descend(simplef, Tuple{Float32, Int32}; view=:typed, optimize=true, interruptexc=false, iswarn=false, terminal)

    displayed, text = read_from(terminal)
    @test occursin("simplef(a, b)", text)
    @test occursin(r"Base\.mul_float\(.*, .*\)::Float32", text)
    @test occursin('[' * colorize(true, 'o') * "]ptimize", displayed)
    @test occursin('[' * colorize(true, 'T') * "]yped", displayed)
    @test occursin('\n' * colorize("Select a call to descend into or ↩ to ascend."; color = :blue), displayed) # beginning of the line
    @test occursin('•', text)
    write(terminal, 'o') # switch to unoptimized
    displayed, text = read_from(terminal)
    @test occursin("simplef(a, b)", text)
    @test occursin("::Const(*)", text)
    @test occursin("(z = (%1)(a, a))", text)
    @test occursin('[' * colorize(false, 'o') * "]ptimize", displayed)
    @test occursin('\n' * colorize("Select a call to descend into or ↩ to ascend."; color = :blue), displayed) # beginning of the line
    @test occursin("• %2 = *(::Float32,::Float32)::Float32", text)

    # Call selection
    write(terminal, keys[:down])
    write(terminal, keys[:enter])
    cread1(terminal)
    displayed, text = read_from(terminal)
    @test occursin(r"• %\d = promote\(::Float32,::Int32\)::Tuple{Float32, Float32}", text)
    write(terminal, keys[:up])
    write(terminal, keys[:enter])
    cread1(terminal)
    displayed, text = read_from(terminal)
    @test occursin("• %2 = *(::Float32,::Float32)::Float32", text) # back to where we started
    write(terminal, 'o') # back to optimized
    displayed, text = read_from(terminal)
    @test !occursin("Variables", text)
    @test occursin(r"Base\.mul_float\(.*, .*\)::Float32", text)
    write(terminal, 'i') # show inline costs
    displayed, text = read_from(terminal)
    @test occursin(r"\d %\d = intrinsic Base\.mul_float", text)
    @test occursin('[' * colorize(true, 'i') * "]nlining costs", displayed)
    write(terminal, 'i') # hide inline costs
    displayed, text = read_from(terminal)
    write(terminal, 'o')
    displayed, text = read_from(terminal)
    @test occursin("Variables", text)
    write(terminal, 'w') # unoptimized + warntype
    displayed, text = read_from(terminal)
    @test occursin("Variables", text)
    @test occursin(r"z.*::Float32", text)
    @test occursin("Body", text)
    @test occursin('[' * colorize(true, 'w') * "]arn", displayed)
    @test occursin('\n' * colorize("Select a call to descend into or ↩ to ascend."; color = :blue), displayed)
    @test occursin("• %2 = *(::Float32,::Float32)::Float32", text)

    # Source view
    write(terminal, 'S')
    displayed, text = read_from(terminal)
    # @test occursin("z\e[36m::Float32\e[39m = (a\e[36m::Float32\e[39m * a\e[36m::Float32\e[39m)\e[36m::Float32\e[39m", displayed)
    @test occursin('[' * colorize(true, 'S') * "]ource", displayed)
    # write(terminal, 's'); cread(terminal) # turn on syntax highlighting
    write(terminal, 's') # turn on syntax highlighting
    displayed, text = read_from(terminal)
    @test occursin("simplef", text)
    @test occursin("\u001B", first(split(displayed, '\n')))
    @test occursin('[' * colorize(true, 's') * "]yntax", displayed)
    write(terminal, 's'); cread(terminal) # turn off syntax highlighting

    # Back to typed code
    write(terminal, 'T'); cread(terminal)
    write(terminal, 'o')
    displayed, text = read_from(terminal)
    @test occursin('[' * colorize(true, 'T') * "]yped", displayed)

    # AST view
    write(terminal, 'A')
    displayed, text = read_from(terminal)
    @test occursin("Symbol simplef", text)
    @test occursin("Symbol call", text)
    @test occursin('[' * colorize(true, 'A') * "]ST", displayed)

    # LLVM view
    write(terminal, 'L')
    displayed, text = read_from(terminal)
    @test occursin(r"sitofp i(64|32)", text)
    @test occursin("fadd float", text)
    @test occursin("┌ @ promotion.jl", text) # debug info on by default
    @test occursin('[' * colorize(true, 'L') * "]LVM", displayed)
    # turn off debug info
    write(terminal, 'd'); cread(terminal)
    write(terminal, 'd'); cread(terminal)
    write(terminal, 'L')
    displayed, text = read_from(terminal)
    @test occursin('[' * colorize(false, 'd') * "]ebuginfo", displayed)
    @test !occursin("┌ @ promotion.jl", text)

    # Native code view
    write(terminal, 'N')
    displayed, text = read_from(terminal)
    @test occursin("retq", text)
    @test occursin('[' * colorize(true, 'N') * "]ative", displayed)
    # Typed view (by selector)
    write(terminal, 'T')
    displayed, text = read_from(terminal)
    @test occursin("Base.mul_float(a, a)::Float32", text)
    @test occursin('[' * colorize(true, 'T') * "]yped", displayed)

    # Parameter dumping
    write(terminal, 'P')
    displayed, text = read_from(terminal)

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
    write(terminal, 'R')
    sleep(0.1)
    write(terminal, 'T'); cread(terminal)
    displayed, text = read_from(terminal)
    @test_broken occursin("z = a * b", displayed)
    @test end_terminal_session(terminal, task)

    # Multicall & iswarn=true
    terminal = FakeTerminal()
    task = @async @with_try_stderr output descend_code_warntype(MultiCall.callfmulti, Tuple{Any}; view=:typed, interruptexc=false, optimize=false, terminal)

    displayed, text = read_from(terminal)
    @test occursin("\nBody", text)
    @test occursin("\e[1m::Union{Float32, $Int}\e[22m\e[39m", displayed)
    @test occursin("Base.getindex(c)\e[91m\e[1m::Any\e[22m\e[39m", displayed)
    warncolor = if _Cthulhu.is_expected_union(Union{Float32, Int64})
        Base.text_colors[Base.warn_color()]
    else
        Base.text_colors[Base.error_color()]
    end
    @test occursin("$(warncolor)%\e[39m3 = call → fmulti(::Any)::Union{Float32, Int64}", displayed)
    write(terminal, keys[:down])
    write(terminal, keys[:enter])
    displayed, text = read_from(terminal)
    @test occursin("%3 = fmulti(::Int32)::Union{Float32, $Int}", displayed)
    @test occursin("%3 = fmulti(::Float32)::Union{Float32, $Int}", displayed)
    @test occursin("%3 = fmulti(::Char)::Union{Float32, $Int}", displayed)
    @test end_terminal_session(terminal, task)

    # Tasks (see the special handling in `_descend`)
    task_function() = @sync @async show(io, "Hello")
    terminal = FakeTerminal()
    task = @async @with_try_stderr output @descend terminal=terminal task_function()

    displayed, text = read_from(terminal)
    @test occursin(r"• %\d\d = task", text)
    write(terminal, keys[:enter])
    displayed, text = read_from(terminal)
    @test occursin("call show(::IO,::String)", text)
    @test end_terminal_session(terminal, task)

    # descend with MethodInstances
    mi = _Cthulhu.get_specialization(MultiCall.callfmulti, Tuple{typeof(Ref{Any}(1))})
    terminal = FakeTerminal()
    task = @async @with_try_stderr output descend(mi; view=:typed, optimize=false, terminal)

    displayed, text = read_from(terminal)
    @test occursin("fmulti(::Any)", text)
    @test end_terminal_session(terminal, task)


    terminal = FakeTerminal()
    task = @async @with_try_stderr output descend_code_warntype(mi; view=:typed, interruptexc=false, optimize=false, terminal)

    displayed, text = read_from(terminal)
    @test occursin("Base.getindex(c)\e[91m\e[1m::Any\e[22m\e[39m", displayed)
    @test end_terminal_session(terminal, task)


    # Fallback to typed code
    terminal = FakeTerminal()
    task = @async @with_try_stderr output redirect_stderr(terminal.error) do
        descend(x -> [x], (Int,); view=:source, interruptexc=false, optimize=false, terminal)
    end

    displayed, text = read_from(terminal)
    warnings = String(readavailable(terminal.error))
    @test occursin("couldn't retrieve source", warnings)
    @test occursin("dynamic Base.vect(x)", text)
    @test occursin("(::$Int)::Vector{$Int}", text)
    @test end_terminal_session(terminal, task)

    # `ascend`
    @noinline inner3(x) = 3x
    @inline   inner2(x) = 2*inner3(x)
                inner1(x) = -1*inner2(x)
    inner1(0x0123)
    mi = _Cthulhu.get_specialization(inner3, Tuple{UInt16})
    terminal = FakeTerminal()
    task = @async @with_try_stderr output redirect_stderr(terminal.error) do
        ascend(terminal, mi; pagesize=11)
    end

    write(terminal, keys[:down])
    write(terminal, keys[:enter])
    write(terminal, keys[:down])
    write(terminal, keys[:enter])
    displayed, text = read_from(terminal)
    lines = split(text, '\n')
    i = first(findfirst("inner3", lines[2]))
    @test first(findfirst("inner2", lines[3])) == i + 2
    from = findfirst(==("Open an editor at a possible caller of"), lines)
    @test isa(from, Int)
    @test occursin("inner2", lines[from + 3])
    write(terminal, 'q')
    @test end_terminal_session(terminal, task)

    # With backtraces
    bt = try sum([]); catch; catch_backtrace(); end
    terminal = FakeTerminal()
    task = @async @with_try_stderr output ascend(terminal, bt)
    write(terminal, keys[:enter])
    displayed, text = read_from(terminal)
    @test occursin("Choose a call for analysis (q to quit):", text)
    @test occursin("mapreduce_empty", text)
    @test occursin("reduce_empty(op::Function, T::Type)", text)
    @test occursin("Select a call to descend into or ↩ to ascend.", text)
    write(terminal, 'q')
    @test end_terminal_session(terminal, task)

    # With stacktraces
    st = try; sum([]); catch; stacktrace(catch_backtrace()); end
    terminal = FakeTerminal()
    task = @async @with_try_stderr output ascend(terminal, st)
    write(terminal, keys[:enter])
    displayed, text = read_from(terminal)
    @test occursin("Choose a call for analysis (q to quit):", text)
    @test occursin("mapreduce_empty", text)
    @test occursin("reduce_empty(op::Function, T::Type)", text)
    @test occursin("Select a call to descend into or ↩ to ascend.", text)
    write(terminal, 'q')
    @test end_terminal_session(terminal, task)

    # With ExceptionStack (e.g., REPL's global `err` variable)
    exception_stack = try; sum([]); catch e; Base.ExceptionStack([(exception=e, backtrace=stacktrace(catch_backtrace()))]); end
    terminal = FakeTerminal()
    task = @async @with_try_stderr output ascend(terminal, exception_stack)
    write(terminal, keys[:enter])
    displayed, text = read_from(terminal)
    @test occursin("Choose a call for analysis (q to quit):", text)
    @test occursin("mapreduce_empty", text)
    @test occursin("reduce_empty(op::Function, T::Type)", text)
    @test occursin("Select a call to descend into or ↩ to ascend.", text)
    write(terminal, 'q')
    @test end_terminal_session(terminal, task)
end

# end # module test_terminal
