struct DefaultProvider <: AbstractProvider
    interp::CthulhuInterpreter
end
DefaultProvider(interp::AbstractInterpreter = NativeInterpreter()) = DefaultProvider(CthulhuInterpreter(interp))

get_abstract_interpreter(provider::DefaultProvider) = provider.interp
AbstractProvider(interp::CthulhuInterpreter) = DefaultProvider(interp)

function should_regenerate_code_instance(provider::DefaultProvider, ci::CodeInstance)
    return !haskey(provider.interp.unopt, ci)
end

get_pc_remarks(provider::DefaultProvider, ci::CodeInstance) = get(provider.interp.remarks, ci, nothing)
get_pc_effects(provider::DefaultProvider, ci::CodeInstance) = get(provider.interp.effects, ci, nothing)
get_pc_excts(provider::DefaultProvider, ci::CodeInstance) = get(provider.interp.exception_types, ci, nothing)
