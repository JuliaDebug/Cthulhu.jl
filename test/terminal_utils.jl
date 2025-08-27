# We use the '↩' character to know when to stop reading, otherwise
# the read will block indefinitely. Note that we do rely on what Cthulhu
# prints, and because we display it twice (once during instructions, once
# at the end), we have to use `cread1` twice.

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
AsyncIO(terminal::FakeTerminal) = AsyncIO(terminal.output)

Base.bytesavailable(io::AsyncIO) = @lock io.lock length(io.buffer)
Base.eof(io::AsyncIO) = !istaskdone(io.task)

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
            written += n
        end
    end
end

cread1(io) = readuntil(io, '↩'; keep=true)
cread(io) = cread1(io) * cread1(io)
strip_ansi_escape_sequences(str) = replace(str, r"\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])" => "")
function read_next(io::AsyncIO)
    displayed = cread(io)
    text = strip_ansi_escape_sequences(displayed)
    return (displayed, text)
end

const keys = Dict(
    :up => "\e[A",
    :down => "\e[B",
    :enter => '\r'
)

macro with_try_stderr(out, expr)
    quote
        try
            $(esc(expr))
        catch err
            bt = catch_backtrace()
            Base.display_error(stderr, err, bt)
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

function end_terminal_session(terminal::FakeTerminal, task::Task, io::AsyncIO)
    wait_for(task, 0.0) && @goto finished
    readavailable(io)
    wait_for(task, 1.0) && @goto finished
    write(terminal, 'q')
    wait_for(task, 1.0) && @goto finished
    readavailable(io)
    @assert wait_for(task)
    @label finished
    finalize(terminal)
    @assert wait_for(io.task, 1.0)
    return istaskdone(task) && istaskdone(io.task)
end
