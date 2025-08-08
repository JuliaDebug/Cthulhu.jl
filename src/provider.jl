struct DefaultProvider <: AbstractProvider
    interp::CthulhuInterpreter
end
DefaultProvider(interp::AbstractInterpreter = NativeInterpreter()) = DefaultProvider(CthulhuInterpreter(interp))

get_abstract_interpreter(provider::DefaultProvider) = provider.interp
AbstractProvider(interp::CthulhuInterpreter) = DefaultProvider(interp)

function should_regenerate_code_instance(provider::DefaultProvider, ci::CodeInstance)
    return !haskey(provider.interp.unopt, ci)
end

get_pc_remarks(provider::DefaultProvider, key::InferenceKey) = get(provider.interp.remarks, key, nothing)
get_pc_effects(provider::DefaultProvider, key::InferenceKey) = get(provider.interp.effects, key, nothing)
get_pc_exct(provider::DefaultProvider, key::InferenceKey) = get(provider.interp.exception_types, key, nothing)
