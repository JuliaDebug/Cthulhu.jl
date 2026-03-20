using Core.IR
using Test
using Cthulhu: Command, CthulhuConfig, CthulhuState, descend, find_callsites,
    find_method_instance, generate_code_instance, get_ci, get_inference_world,
    get_inlining_costs, get_override, get_pc_effects, get_pc_excts, get_pc_remarks,
    is_command_enabled, lookup, menu_commands, show_command, show_parameters
using .CompilerIntegration: DefaultProvider, LookupResult, PC2Effects, PC2Excts, PC2Remarks
using Cthulhu.Testing: @run, TestHarness, VirtualTerminal, end_terminal_session, read_next,
    wait_for
using Logging: NullLogger, with_logger

function test_provider_api(provider, args...)
    world = get_inference_world(provider)
    @test isa(world, UInt)
    mi = find_method_instance(provider, args..., world)
    @test isa(mi, MethodInstance)
    ci = generate_code_instance(provider, mi)
    @test isa(ci, CodeInstance)
    result = lookup(provider, ci, false)
    @test isa(result, LookupResult)
    result = lookup(provider, ci, true)
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
            with_logger(NullLogger()) do
                output = sprint(show_parameters, provider)
                @test isa(output, String)
            end
        end
    end

    with_logger(NullLogger()) do
        effects = get_pc_effects(provider, ci)
        @test effects === nothing || isa(effects, PC2Effects)
        remarks = get_pc_remarks(provider, ci)
        @test remarks === nothing || isa(remarks, PC2Remarks)
        excts = get_pc_excts(provider, ci)
        @test excts === nothing || isa(excts, PC2Excts)
        inlining_costs = get_inlining_costs(provider, mi, something(result.src, result.ir))
        @test inlining_costs === nothing || isa(inlining_costs, Vector{Int})
    end

    result = lookup(provider, ci, false)
    callsites, _ = find_callsites(provider, result, ci)
    @test length(callsites) ≥ ifelse(result.optimized, 1, 5)
    for callsite in callsites
        ci = get_ci(callsite)
        ci === nothing && continue
        override = get_override(provider, callsite.info)
        src = something(override, ci)
        result = lookup(provider, src, false)
        @test isa(result, LookupResult)
        result = lookup(provider, src, true)
        @test isa(result, LookupResult)
    end
end

function default_config_kwargs()
    config = CthulhuConfig()
    return NamedTuple(name => getfield(config, name) for name in propertynames(config))
end

function test_descend_for_provider(provider, args...; show = false)
    terminal = VirtualTerminal()
    harness = @run terminal descend(args...; terminal, provider, default_config_kwargs()...)
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
    write(terminal, :enter)
    write(terminal, 'S')
    write(terminal, :up)
    write(terminal, :enter)
    write(terminal, 'q')
    if show
        wait_for(harness.task)
        displayed = String(readavailable(harness.io))
        println(displayed)
    end
    @test end_terminal_session(harness)
end
