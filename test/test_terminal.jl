module test_terminal

using Core: Const
using Test, REPL, LinearAlgebra
using Cthulhu.Testing
using Cthulhu.Testing: @run
using Cthulhu: Cthulhu as _Cthulhu, is_compiler_loaded, descend, descend_code_warntype, @descend, ascend
const Cthulhu = _Cthulhu.CTHULHU_MODULE[]

if isdefined(parentmodule(@__MODULE__), :VSCodeServer)
    using ..VSCodeServer
end

# For multi-call sites
module Definitions
fmulti(::Int32) = 1
fmulti(::Float32) = rand(Float32)
fmulti(::Char) = 3
callfmulti(c) = fmulti(c[])

reduce_tuple(@nospecialize(ixs)) = ixs
function reduce_tuple(ixs::Tuple)
    values = ntuple(length(ixs)) do i
        @inline
        reduce_tuple(ixs[i])
    end
    return prod(values)
end
end

if is_compiler_loaded()
    @eval using Revise
    Revise.track(Base) # get the `@info` log now, to avoid polluting test outputs later
    revised_file = tempname() * ".jl"
    open(revised_file, "w+") do io
        println(io, """
        module Sandbox
        function simplef(a, b)
            z = a * a
            return z + b
        end
        end # module
        """)
    end
    include(revised_file)
    (; simplef) = Sandbox
    Revise.track(Sandbox, revised_file)
else
    @eval function simplef(a, b)
        z = a * a
        return z + b
    end
end

@testset "Terminal" begin
    @test Cthulhu.default_terminal() isa REPL.Terminals.TTYTerminal
    colorize(active_option::Bool, c::Char) = Cthulhu.stringify() do io
        active_option ? printstyled(io, c; bold=true, color=:green) : printstyled(io, c; color=:red)
    end

    colorize(s::AbstractString; color::Symbol = :cyan) = Cthulhu.stringify() do io
        printstyled(io, s; color)
    end

    Cthulhu.CONFIG = Cthulhu.CthulhuConfig(; menu_options = (; pagesize = 10000))

    terminal = VirtualTerminal()
    harness = @run terminal descend(simplef, Tuple{Float32, Int32}; view=:typed, optimize=true, iswarn=false, terminal)

    displayed, text = read_next(harness)
    @test occursin("simplef(a, b)", text)
    @test occursin(r"Base\.mul_float\(.*, .*\)::Float32", text)
    @test occursin('[' * colorize(true, 'o') * "]ptimize", displayed)
    @test occursin('[' * colorize(true, 'T') * "]yped", displayed)
    @test occursin('\n' * colorize("Select a call to descend into or ↩ to ascend."; color = :blue), displayed) # beginning of the line
    @test occursin('•', text)
    write(terminal, 'o') # switch to unoptimized
    displayed, text = read_next(harness)
    @test occursin("simplef(a, b)", text)
    if @isdefined(Revise)
        @test occursin("::Const(*)", text)
    end
    @test occursin("(z = (%1)(a, a))", text)
    @test occursin('[' * colorize(false, 'o') * "]ptimize", displayed)
    @test occursin('\n' * colorize("Select a call to descend into or ↩ to ascend."; color = :blue), displayed) # beginning of the line
    @test occursin("• %2 = *(::Float32,::Float32)::Float32", text)

    # Call selection
    write(terminal, :down)
    skip_delimiter(harness)
    write(terminal, :enter)
    displayed, text = read_next(harness)
    @test occursin(r"• %\d = promote\(::Float32,::Int32\)::Tuple{Float32, Float32}", text)
    write(terminal, :up)
    skip_delimiter(harness)
    write(terminal, :enter)
    displayed, text = read_next(harness)
    @test occursin("• %2 = *(::Float32,::Float32)::Float32", text) # back to where we started
    write(terminal, 'o') # back to optimized
    displayed, text = read_next(harness)
    @test !occursin("Variables", text)
    @test occursin(r"Base\.mul_float\(.*, .*\)::Float32", text)
    write(terminal, 'i') # show inline costs
    displayed, text = read_next(harness)
    @test occursin(r"\d %\d = intrinsic Base\.mul_float", text)
    @test occursin('[' * colorize(true, 'i') * "]nlining costs", displayed)
    write(terminal, 'i') # hide inline costs
    displayed, text = read_next(harness)
    write(terminal, 'o')
    displayed, text = read_next(harness)
    @test occursin("Variables", text)
    write(terminal, 'w') # unoptimized + warntype
    displayed, text = read_next(harness)
    @test occursin("Variables", text)
    @test occursin(r"z.*::Float32", text)
    @test occursin("Body", text)
    @test occursin('[' * colorize(true, 'w') * "]arn", displayed)
    @test occursin('\n' * colorize("Select a call to descend into or ↩ to ascend."; color = :blue), displayed)
    @test occursin("• %2 = *(::Float32,::Float32)::Float32", text)

    # Source view
    write(terminal, 'S')
    displayed, text = read_next(harness)
    # @test occursin("z\e[36m::Float32\e[39m = (a\e[36m::Float32\e[39m * a\e[36m::Float32\e[39m)\e[36m::Float32\e[39m", displayed)
    @test occursin('[' * colorize(true, 'S') * "]ource", displayed)
    write(terminal, 's') # turn on syntax highlighting
    displayed, text = read_next(harness)
    @test occursin("simplef", text)
    @test occursin("\u001B", first(split(displayed, '\n')))
    @test occursin('[' * colorize(true, 's') * "]yntax", displayed)
    write(terminal, 's'); read_next(harness) # turn off syntax highlighting

    # Back to typed code
    write(terminal, 'T'); read_next(harness)
    write(terminal, 'o')
    displayed, text = read_next(harness)
    @test occursin('[' * colorize(true, 'T') * "]yped", displayed)

    # AST view
    write(terminal, 'A')
    displayed, text = read_next(harness)
    if @isdefined(Revise)
        @test occursin("Symbol simplef", text)
        @test occursin("Symbol call", text)
    end
    @test occursin('[' * colorize(true, 'A') * "]ST", displayed)

    # LLVM view
    write(terminal, 'L')
    displayed, text = read_next(harness)
    @test occursin(r"sitofp i(64|32)", text)
    @test occursin("fadd float", text)
    @test occursin("┌ @ promotion.jl", text) # debug info on by default
    @test occursin('[' * colorize(true, 'L') * "]LVM", displayed)
    # turn off debug info
    write(terminal, 'd'); read_next(harness)
    write(terminal, 'd'); read_next(harness)
    write(terminal, 'L')
    displayed, text = read_next(harness)
    @test occursin('[' * colorize(false, 'd') * "]ebuginfo", displayed)
    @test !occursin("┌ @ promotion.jl", text)

    # Native code view
    write(terminal, 'N')
    displayed, text = read_next(harness)
    @test occursin("retq", text)
    @test occursin('[' * colorize(true, 'N') * "]ative", displayed)
    # Typed view (by selector)
    write(terminal, 'T')
    displayed, text = read_next(harness)
    @test occursin("Base.mul_float(a, a)::Float32", text)
    @test occursin('[' * colorize(true, 'T') * "]yped", displayed)

    # Parameter dumping
    write(terminal, 'P')
    displayed, text = read_next(harness)

    if @isdefined(Revise)
        # Use delays to ensure unambiguous differences in time stamps
        # (macOS is particularly sensitive) and execution of @async processes
        sleep(0.1)
        open(revised_file, "w") do io
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
        displayed, text = read_next(harness)
        # FIXME: Sources are revised, but Cthulhu still displays the unrevised code.
        @test_broken occursin("Base.mul_float(a, b)::Float32", text)
        write(terminal, 'S')
        displayed, text = read_next(harness)
        @test_broken occursin("z::Float32 = (a::Float32 * b::Int32)", text)
        # FIXME: Replace this test by the one marked as broken just above when fixed.
        @test occursin("z::Float32 = (a::Float32 * b)", text)
        @test end_terminal_session(harness)
    end

    # Multicall & iswarn=true
    terminal = VirtualTerminal()
    harness = @run terminal descend_code_warntype(Definitions.callfmulti, Tuple{Any}; view=:typed, optimize=false, terminal)

    displayed, text = read_next(harness)
    @test occursin("\nBody", text)
    @test occursin("\e[1m::Union{Float32, $Int}\e[22m\e[39m", displayed)
    @test occursin("Base.getindex(c)\e[91m\e[1m::Any\e[22m\e[39m", displayed)
    warncolor = if Cthulhu.is_expected_union(Union{Float32, Int64})
        Base.text_colors[Base.warn_color()]
    else
        Base.text_colors[Base.error_color()]
    end
    @test occursin("$(warncolor)%\e[39m3 = call → fmulti(::Any)::Union{Float32, Int64}", displayed)
    write(terminal, :down)
    write(terminal, :enter)
    displayed, text = read_next(harness)
    @test occursin("%3 = fmulti(::Int32)::Union{Float32, $Int}", displayed)
    @test occursin("%3 = fmulti(::Float32)::Union{Float32, $Int}", displayed)
    @test occursin("%3 = fmulti(::Char)::Union{Float32, $Int}", displayed)
    @test end_terminal_session(harness)

    # Tasks (see the special handling in `_descend`)
    task_function() = @sync @async show(io, "Hello")
    terminal = VirtualTerminal()
    harness = @run terminal @descend terminal=terminal view=:typed optimize=true task_function()

    displayed, text = read_next(harness)
    @test occursin(r"• %\d\d = task", text)
    write(terminal, :enter)
    displayed, text = read_next(harness)
    @test occursin("call show(::IO,::String)", text)
    @test end_terminal_session(harness)

    # descend with MethodInstances
    mi = _Cthulhu.get_specialization(Definitions.callfmulti, Tuple{typeof(Ref{Any}(1))})
    terminal = VirtualTerminal()
    harness = @run terminal descend(mi; view=:typed, optimize=false, terminal)

    displayed, text = read_next(harness)
    @test occursin("fmulti(::Any)", text)
    @test end_terminal_session(harness)

    terminal = VirtualTerminal()
    harness = @run terminal descend_code_warntype(mi; view=:typed, optimize=false, terminal)

    displayed, text = read_next(harness)
    @test occursin("Base.getindex(c)\e[91m\e[1m::Any\e[22m\e[39m", displayed)
    @test end_terminal_session(harness)


    # Fallback to typed code
    @test_logs (:warn, r"couldn't retrieve source") match_mode=:any begin
        terminal = VirtualTerminal()
        harness = @run terminal descend(x -> [x], (Int,); view=:source, optimize=false, terminal)

        displayed, text = read_next(harness)
        @test occursin("dynamic Base.vect(x)", text)
        @test occursin("(::$Int)::Vector{$Int}", text)
        @test end_terminal_session(harness)
    end

    @testset "Source discarded because of LimitedAccuracy (#642)" begin
        @test_logs (:warn, r"Inference decided not to cache") match_mode=:any begin
            terminal = VirtualTerminal()
            harness = @run terminal descend(Definitions.reduce_tuple, (typeof(((1, (1, 1)), 1)),); terminal)
            displayed, text = read_next(harness)
            write(terminal, 'T')
            displayed, text = read_next(harness)
            write(terminal, :down)
            skip_delimiter(harness)
            write(terminal, :down)
            skip_delimiter(harness)
            write(terminal, :enter)
            displayed, text = read_next(harness)
            write(terminal, 'o')
            @test end_terminal_session(harness)
        end
    end

    @testset "Code instances from [semi-]concrete evaluation (#609, #610)" begin
        U = UpperTriangular{Int64, Matrix{Int64}}
        S = Symmetric{Int64, Matrix{Int64}}
        terminal = VirtualTerminal()
        harness = @run terminal descend(*, (U, S); terminal)
        displayed, text = read_next(harness)
        write(terminal, :enter)
        displayed, text = read_next(harness)
        write(terminal, :up)
        skip_delimiter(harness)
        write(terminal, :up)
        skip_delimiter(harness)
        write(terminal, :enter)
        displayed, text = read_next(harness)
        write(terminal, :enter)
        displayed, text = read_next(harness)
        write(terminal, :enter)
        displayed, text = read_next(harness)
        VERSION < v"1.13-" && @test contains(text, "_trimul!")
        @test end_terminal_session(harness)
    end

    # `ascend`
    @noinline inner3(x) = 3x
    @inline   inner2(x) = 2*inner3(x)
              inner1(x) = -1*inner2(x)
    inner1(0x0123)
    mi = _Cthulhu.get_specialization(inner3, Tuple{UInt16})
    terminal = VirtualTerminal()
    harness = @run terminal ascend(terminal, mi; pagesize=11)

    write(terminal, :down)
    write(terminal, :enter)
    write(terminal, :down)
    write(terminal, :enter)
    displayed, text = read_next(harness)
    lines = split(text, '\n')
    i = first(findfirst("inner3", lines[2]))
    @test first(findfirst("inner2", lines[3])) == i + 2
    from = findfirst(==("Open an editor at a possible caller of"), lines)
    @test isa(from, Int)
    @test occursin("inner2", lines[from + 3])
    write(terminal, 'q')
    @test end_terminal_session(harness)

    # With backtraces
    bt = try sum([]); catch; catch_backtrace(); end
    terminal = VirtualTerminal()
    harness = @run terminal ascend(terminal, bt)
    write(terminal, :enter)
    displayed, text = read_next(harness)
    @test occursin("Choose a call for analysis (q to quit):", text)
    @test occursin("mapreduce_empty", text)
    @test occursin("reduce_empty(op::Function, T::Type)", text) || occursin("reduce_empty(::typeof(+), ::Type{Any})", text)
    @test occursin("Select a call to descend into or ↩ to ascend.", text)
    write(terminal, 'q')
    @test end_terminal_session(harness)

    # With stacktraces
    st = try; sum([]); catch; stacktrace(catch_backtrace()); end
    terminal = VirtualTerminal()
    harness = @run terminal ascend(terminal, st)
    write(terminal, :enter)
    displayed, text = read_next(harness)
    @test occursin("Choose a call for analysis (q to quit):", text)
    @test occursin("mapreduce_empty", text)
    @test occursin("reduce_empty(op::Function, T::Type)", text) || occursin("reduce_empty(::typeof(+), ::Type{Any})", text)
    @test occursin("Select a call to descend into or ↩ to ascend.", text)
    write(terminal, 'q')
    @test end_terminal_session(harness)

    # With ExceptionStack (e.g., REPL's global `err` variable)
    exception_stack = try; sum([]); catch e; Base.ExceptionStack([(exception=e, backtrace=stacktrace(catch_backtrace()))]); end
    terminal = VirtualTerminal()
    harness = @run terminal ascend(terminal, exception_stack)
    write(terminal, :enter)
    displayed, text = read_next(harness)
    @test occursin("Choose a call for analysis (q to quit):", text)
    @test occursin("mapreduce_empty", text)
    @test occursin("reduce_empty(op::Function, T::Type)", text) || occursin("reduce_empty(::typeof(+), ::Type{Any})", text)
    @test occursin("Select a call to descend into or ↩ to ascend.", text)
    write(terminal, 'q')
    @test end_terminal_session(harness)

    @testset "Bookmarks" begin
        bookmarks = Cthulhu.BOOKMARKS
        n = length(bookmarks)

        @test_logs (:info, r"`descend` state was saved for later use") match_mode=:any begin
            terminal = VirtualTerminal()
            harness = @run terminal descend(exp, (Int,); view=:typed, optimize=true, terminal)
            write(terminal, :enter)
            write(terminal, 'b') # should push a bookmark to `BOOKMARKS`
            write(terminal, 'q')
            @test end_terminal_session(harness)
        end

        @test length(bookmarks) == n + 1
        bookmark = pop!(bookmarks)
        mi = Cthulhu.get_mi(bookmark.ci)
        method = mi.def
        @test method === which(exp, (Float64,))
        @test bookmark.config.view === :typed
        @test bookmark.config.optimize === true

        terminal = VirtualTerminal()
        @test_logs (:info, r"`descend` state was saved for later use") match_mode=:any begin
            harness = @run terminal Cthulhu.descend_with_error_handling(bookmark; view=:llvm, terminal)
            write(terminal, 'b') # should push a bookmark to `BOOKMARKS`
            write(terminal, 'q')
            @test end_terminal_session(harness)
        end

        bookmark = pop!(bookmarks)
        @test bookmark.config.view === :llvm
    end
end;

end # module test_terminal
