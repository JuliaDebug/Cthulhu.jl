module Testing

using REPL.Terminals: TTYTerminal, UnixTerminal
using REPL: TerminalMenus

"""
Terminal implementation that connects virtual input/output/error pipes
to be interacted with asynchronously with a program.
"""
mutable struct VirtualTerminal <: UnixTerminal
    input::Pipe
    output::Pipe
    error::Pipe
    tty::TTYTerminal
    in_stream::IO
    out_stream::IO
    err_stream::IO
    function VirtualTerminal(; context = [:color => get(stdout, :color, false)])
        input = Pipe()
        output = Pipe()
        error = Pipe()
        Base.link_pipe!(input, reader_supports_async=true, writer_supports_async=true)
        Base.link_pipe!(output, reader_supports_async=true, writer_supports_async=true)
        Base.link_pipe!(error, reader_supports_async=true, writer_supports_async=true)
        term_env = get(ENV, "TERM", @static Sys.iswindows() ? "" : "dumb")
        in_stream = input.out
        out_stream = IOContext(output.in, context...)
        err_stream = error.in
        tty = TTYTerminal(term_env, in_stream, out_stream, err_stream)
        terminal = new(input, output, error, tty, in_stream, out_stream, err_stream)
        return finalizer(terminal) do x
            @async begin
                close(x.input)
                close(x.output)
                close(x.error)
            end
        end
    end
end

Base.write(terminal::VirtualTerminal, key::Symbol) = write(terminal, KEYS[key])
Base.write(terminal::VirtualTerminal, char::Char) = write(terminal.input, char)
Base.write(terminal::VirtualTerminal, input::String) = write(terminal.input, input)
Base.read(terminal::VirtualTerminal, args...; error = false) = read(ifelse(error, terminal.error, terminal.output), args...)
TerminalMenus.request(terminal::VirtualTerminal, args...; kwargs...) = TerminalMenus.request(terminal.tty, args...; kwargs...)

"""
IO subtype that continuously reads another `IO` object in a non-blocking way
using an asynchronously-running task and puts the results into a byte buffer.

The intended use case is to ensure writes to the wrapped `IO` are always read
to reliably avoid blocking future writes past a certain amount.
"""
struct AsyncIO <: IO
    io::IO
    buffer::Vector{UInt8}
    lock::ReentrantLock
    task::Task
end

function AsyncIO(io::IO)
    buffer = UInt8[]
    lock = ReentrantLock()
    task = @async while !eof(io)
        available = readavailable(io)
        while bytesavailable(io) > 0
            append!(available, readavailable(io))
        end
        @lock lock append!(buffer, available)
    end
    return AsyncIO(io, buffer, lock, task)
end
AsyncIO(terminal::VirtualTerminal) = AsyncIO(terminal.output)

function Base.peek(io::AsyncIO, ::Type{UInt8})
    while bytesavailable(io) < 1 yield() end
    return @lock io.lock io.buffer[1]
end
function Base.read(io::AsyncIO, ::Type{UInt8})
    ref = Ref(0x00)
    GC.@preserve ref begin
        ptr = Base.unsafe_convert(Ptr{UInt8}, ref)
        unsafe_read(io, ptr, 1)
    end
    return ref[]
end
Base.bytesavailable(io::AsyncIO) = @lock io.lock length(io.buffer)
Base.eof(io::AsyncIO) = istaskdone(io.task) && @lock io.lock isempty(io.buffer)

function Base.readavailable(io::AsyncIO)
    bytes = UInt8[]
    @lock io.lock begin
        append!(bytes, io.buffer)
        empty!(io.buffer)
    end
    return bytes
end

function Base.unsafe_read(io::AsyncIO, to::Ptr{UInt8}, nb::UInt)
    written = 0
    while written < nb
        bytesavailable(io) > 0 || (yield(); continue)
        @lock io.lock begin
            (; buffer) = io
            n = min(length(buffer), nb)
            GC.@preserve buffer begin
                unsafe_copyto!(to, pointer(buffer), n)
            end
            splice!(buffer, 1:n)
            written += n
        end
        istaskdone(io.task) && throw(EOFError())
    end
end

# We use the '↩' character as a delimiter for UI displays (this is
# specific to Cthulhu, the last character we emit is that one).
# Because we print it twice with Cthulhu (once for menu options, once
# at the end for callsite selection), we effectively use twice-'↩' as a delimiter.
cread1(io::AsyncIO) = readuntil(io, '↩'; keep=true)
cread(io::AsyncIO) = cread1(io) * cread1(io)
strip_ansi_escape_sequences(str) = replace(str, r"\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])" => "")
function read_next(io::AsyncIO)
    displayed = cread(io)
    text = strip_ansi_escape_sequences(displayed)
    return (displayed, text)
end

"""
A testing utility that correctly wraps a [`VirtualTerminal`](@ref)
with an asynchronous IO reader to avoid blocking while asynchronously
executing a task that interacts with the terminal.

This harness:
- Protects the terminal's input from a blocking congestion (caused by waiting for a read on its output).
- Logs any task errors to `stderr` to avoid silent failures.

In an ideal world, we wouldn't need a utility like that, but it so happens that
emulated asynchronous terminal interactions are tricky especially when we rely on
testing unstructured IO output to be semantically delimited into regions.
"""
mutable struct TestHarness
    terminal::VirtualTerminal
    io::AsyncIO
    task::Task
    function TestHarness(terminal::VirtualTerminal)
        io = AsyncIO(terminal)
        harness = new(terminal, io)
    end
end

macro display_errors(expr)
    quote
        try
            $(esc(expr))
        catch err
            bt = catch_backtrace()
            # print all at once, to avoid interleaving output with other prints
            output = sprint(Base.display_error, err, bt; context = :color => true)
            println(output)
        end
    end
end

function Base.run(harness::TestHarness, task::Task)
    !isdefined(harness, :task) || error("A task was already executed with this harness")
    harness.task = task
end

macro run(terminal, ex)
    quote
        terminal = $(esc(terminal))
        harness = TestHarness(terminal)
        task = @async @display_errors $(esc(ex))
        run(harness, task)
        harness
    end
end

function read_next(harness::TestHarness)
    istaskdone(harness.task) && error("The task is not running")
    return read_next(harness.io)
end

function skip_delimiter(harness::TestHarness)
    cread1(harness.io)
    return nothing
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

end_terminal_session(harness::TestHarness) =
    end_terminal_session(harness.terminal, harness.task, harness.io)

function end_terminal_session(terminal::VirtualTerminal, task::Task, io::AsyncIO)
    wait_for(task, 0.0) && @goto finished
    write(terminal, 'q')
    wait_for(task, 1.0) && @goto finished
    write(terminal, 'q')
    wait_for(task, 1.0) && @goto finished
    @assert wait_for(task)
    @label finished
    finalize(terminal)
    @assert wait_for(io.task, 1.0)
    return istaskdone(task) && istaskdone(io.task)
end

const KEYS = Dict(:up => "\e[A",
                  :down => "\e[B",
                  :enter => '\r')

export VirtualTerminal, TestHarness, @run, read_next, end_terminal_session, skip_delimiter

end # module Testing
