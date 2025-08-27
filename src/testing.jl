module Testing

using REPL.Terminals: TTYTerminal, UnixTerminal
using REPL: TerminalMenus
import Base: read, write

mutable struct FakeTerminal <: UnixTerminal
    # Use pipes so we can easily do blocking reads.
    input::Pipe
    output::Pipe
    error::Pipe
    tty::TTYTerminal
    in_stream::IO
    out_stream::IO
    err_stream::IO
    function FakeTerminal(; context = [:color => get(stdout, :color, false)])
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

Base.write(terminal::FakeTerminal, char::Char) = write(terminal.input, char)
Base.write(terminal::FakeTerminal, input::String) = write(terminal.input, input)
Base.read(terminal::FakeTerminal, args...; error = false) = read(ifelse(error, terminal.error, terminal.output), args...)
TerminalMenus.request(terminal::FakeTerminal, args...; kwargs...) = TerminalMenus.request(terminal.tty, args...; kwargs...)

export FakeTerminal

end # module Testing
