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

function end_terminal_session(terminal, task)
    wait_for(task, 0) && @goto finished
    readavailable(terminal.output)
    wait_for(task, 0) && @goto finished
    write(terminal, 'q')
    readavailable(terminal.output)
    @assert wait_for(task)
    @label finished
    finalize(terminal)
    return istaskdone(task)
end
