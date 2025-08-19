# module test_provider

using Test
using Core.IR
import Cthulhu as _Cthulhu
global Cthulhu::Module = _Cthulhu.CTHULHU_MODULE[]
using Cthulhu: descend
using .Cthulhu: CC, DefaultProvider, get_inference_world, find_method_instance, generate_code_instance, should_regenerate_code_instance, get_override, LookupResult, Command, menu_commands, is_command_enabled, show_command, CthulhuState, PC2Effects, get_pc_effects, PC2Remarks, get_pc_remarks, PC2Excts, get_pc_excts, get_inlining_costs, show_parameters
using .Cthulhu.Testing: FakeTerminal

include("terminal_utils.jl")

function test_provider_api(provider, args...)
    world = get_inference_world(provider)
    @test isa(world, UInt)
    mi = find_method_instance(provider, args..., world)
    @test isa(mi, MethodInstance)
    ci = generate_code_instance(provider, mi)
    @test isa(ci, CodeInstance)
    ret = should_regenerate_code_instance(provider, ci)
    @test isa(ret, Bool)
    override = get_override(provider, CC.NoCallInfo())
    @test isa(override, Any)
    src = something(override, ci)
    result = LookupResult(provider, src, false)
    @test isa(result, LookupResult)
    result = LookupResult(provider, src, true)
    @test isa(result, LookupResult)

    commands = menu_commands(provider)
    @test isa(commands, Vector{Command})
    state = CthulhuState(provider; ci, mi)
    @test any(command -> is_command_enabled(provider, state, command) === true, commands)
    for command in commands
        output = sprint(show_command, provider, state, command; context = :color => true)
        @test isa(output, String)
        @test contains(output, Char(command.key))
        if command.name === :dump_params
            output = sprint(show_parameters, provider)
            @test isa(output, String)
        end
    end

    effects = get_pc_effects(provider, ci)
    @test effects === nothing || isa(effects, PC2Effects)
    remarks = get_pc_remarks(provider, ci)
    @test remarks === nothing || isa(remarks, PC2Remarks)
    excts = get_pc_excts(provider, ci)
    @test excts === nothing || isa(excts, PC2Excts)
    inlining_costs = get_inlining_costs(provider, mi, something(result.src, result.ir))
    @test inlining_costs === nothing || isa(inlining_costs, Vector{Int})
end

function test_descend_for_provider(provider, args...)
    terminal = FakeTerminal()
    task = @async @with_try_stderr terminal.output descend(provider, args...; terminal)
    write(terminal, 'T')
    write(terminal, 'o') # optimize: on
    write(terminal, 'L')
    write(terminal, 'd') # debuginfo: :source
    write(terminal, 'd') # debuginfo: :compact
    write(terminal, 'T')
    write(terminal, 'd') # debuginfo: :none
    write(terminal, 'S') # optimize: off
    write(terminal, 'T')
    write(terminal, 'r') # remarks: on
    write(terminal, 'e') # effects: on
    write(terminal, 'o') # optimize: on
    write(terminal, 'i') # inlining costs: on
    write(terminal, 'o') # optimize: off
    write(terminal, keys[:down])
    write(terminal, keys[:enter])
    write(terminal, 'T')
    write(terminal, 'q')
    println(terminal.out_stream, '⇜')
    readuntil(terminal.output, '⇜')
    @test end_terminal_session(terminal, task)
end

include("providers/CountingProviderModule.jl")
using .CountingProviderModule: CountingProvider
include("providers/OverlayProviderModule.jl")
using .OverlayProviderModule: OverlayProvider

@testset "Example providers" begin
    args = (exp, (Float64,))

    @testset "Provider API" begin
        test_provider_api(DefaultProvider(), args...)
        test_provider_api(CountingProvider(), args...)
        test_provider_api(OverlayProvider(), args...)
    end

    @testset "`descend`" begin
        # FIXME: this hangs
        # test_descend_for_provider(DefaultProvider(), args...)
        # test_descend_for_provider(CountingProvider(), args...)
        # test_descend_for_provider(OverlayProvider(), args...)
    end
end

# end # module test_provider
