module FakeTerminals

import REPL
using Test

export fake_terminal

function fake_terminal(f; options::REPL.Options=REPL.Options(confirm_exit=false))
    # Use pipes so we can easily do blocking reads
    # In the future if we want we can add a test that the right object
    # gets displayed by intercepting the display
    input = Pipe()
    output = Pipe()
    err = Pipe()
    Base.link_pipe!(input, reader_supports_async=true, writer_supports_async=true)
    Base.link_pipe!(output, reader_supports_async=true, writer_supports_async=true)
    Base.link_pipe!(err, reader_supports_async=true, writer_supports_async=true)

    term_env = get(ENV, "TERM", @static Sys.iswindows() ? "" : "dumb")
    term = REPL.Terminals.TTYTerminal(term_env, input.out, IOContext(output.in, :color=>get(stdout, :color, false)), err.in)
    f(term, input.in, output.out, err)
    t = @async begin
        close(input.in)
        close(output.in)
        close(err.in)
    end
    @test read(err.out, String) == ""
    wait(t)
end

end
