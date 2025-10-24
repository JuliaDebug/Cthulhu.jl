module CountingProviderModule

using Core.IR

import ..CompilerIntegration
using ..CompilerIntegration: CC, DefaultProvider, CthulhuInterpreter, OptimizedSource, InferredSource
using .CC: InferenceResult

using Cthulhu: Cthulhu, AbstractProvider, generate_code_instance, Command, default_menu_commands, run_type_inference

mutable struct CountingProvider <: AbstractProvider
    default::DefaultProvider
    count::Int
    CountingProvider() = new(DefaultProvider(), 0)
end

Cthulhu.get_abstract_interpreter(provider::CountingProvider) =
    Cthulhu.get_abstract_interpreter(provider.default)

function Cthulhu.generate_code_instance(provider::CountingProvider, mi::MethodInstance)
    provider.count += 1
    return generate_code_instance(provider.default, mi)
end

function Cthulhu.menu_commands(provider::CountingProvider)
    commands = default_menu_commands(provider)
    append!(commands, [
        modify_count('+', :increment,  1),
        modify_count('-', :decrement, -1),
    ])
    return commands
end

function modify_count(key, name, value)
    Command(state -> state.provider.count += value, key, name, string(name), :actions)
end

CompilerIntegration.run_type_inference(provider::CountingProvider, interp::CthulhuInterpreter, mi::MethodInstance) =
    run_type_inference(provider.default, interp, mi)
CompilerIntegration.OptimizedSource(provider::CountingProvider, interp::CthulhuInterpreter, ci::CodeInstance) =
    OptimizedSource(provider.default, interp, ci)
CompilerIntegration.OptimizedSource(provider::CountingProvider, interp::CthulhuInterpreter, result::InferenceResult) =
    OptimizedSource(provider.default, interp, result)
CompilerIntegration.InferredSource(provider::CountingProvider, interp::CthulhuInterpreter, ci::CodeInstance) =
    InferredSource(provider.default, interp, ci)

export CountingProvider

end
