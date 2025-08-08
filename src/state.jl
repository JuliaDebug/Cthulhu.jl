mutable struct CthulhuState
    terminal::AbstractTerminal
    provider::AbstractProvider
    config::CthulhuConfig
    mi::Union{Nothing, MethodInstance}
    ci::Union{Nothing, CodeInstance}
    override::Any
    display_code::Bool
end
function CthulhuState(provider::AbstractProvider; terminal = default_terminal(), config = CONFIG,
                      mi = nothing, ci = nothing, override = nothing)
    return CthulhuState(terminal, provider, config, mi, ci, override, true)
end

view_function(state::CthulhuState) = view_function(state.provider, state.config.view)
function view_function(provider::AbstractProvider, view::Symbol)
    view === :source && return cthulhu_source
    view === :typed && return cthulhu_typed
    view === :ast && return cthulhu_ast
    view === :llvm && return cthulhu_llvm
    view === :native && return cthulhu_native
    throw_view_is_not_supported(provider, view)
end

throw_view_is_not_supported(provider, view) = throw(ArgumentError("View `$view` is not supported for provider $(typeof(provider))"))

mutable struct Command
    active::Union{Nothing, Bool}
    key::UInt32
    name::Symbol
    description::String
    category::Symbol
    callback_on::Any
    callback_off::Any
    function Command(active::Union{Nothing, Bool}, key, name, description, category, @nospecialize(callback_on), @nospecialize(callback_off))
        key = convert(UInt32, key)
        description = convert(String, description)
        return new(active, key, name, description, category, callback_on, callback_off)
    end
end

function default_menu_commands()
    commands = Command[
        set_option('w', :iswarn, :toggles, "warn"),
        set_option('h', :hide_type_stable, :toggles, "hide type-stable statements"),
        set_option('o', :optimize, :toggles),
        set_option('d', :debuginfo, :toggles),
        set_option('r', :remarks, :toggles),
        set_option('e', :with_effects, :toggles, "effects"),
        set_option('x', :exception_type, :toggles),
        set_option('i', :inline_cost, :toggles, "inlining costs"),
        set_option('t', :type_annotations, :toggles),
        set_option('s', :enable_highlighter, :toggles, "syntax highlight for Source/LLVM/Native"),
        set_option('v', :inlay_types_vscode, :toggles, "vscode: inlay types"),
        set_option('V', :diagnostics_vscode, :toggles, "vscode: diagnostics"),
        set_option('j', :jump_always, :toggles, "jump to source always"; redisplay = false),
        set_view('S', :source, :show),
        set_view('A', :ast, :show),
        set_view('T', :typed, :show),
        set_view('L', :llvm, :show),
        set_view('N', :native, :show),
        perform_action(edit_source_code, 'E', :edit, :actions, "Edit source code"),
        perform_action(revise_and_redisplay!, 'R', :revise, :actions, "Revise and redisplay"),
        perform_action(bookmark_method, 'b', :bookmark, :actions),
        perform_action(dump_parameters, 'P', :dump_params, :actions, "dump params cache"),
        perform_action(nothing, 'q', :quit, :actions),
    ]
    return commands
end

function perform_action(f, key::Char, name::Symbol, category, description = string(name))
    return Command(nothing, key, name, description, category, f, nothing)
end

function set_option(key::Char, option::Symbol, category, description = string(option); redisplay = true)
    return Command(false, key, option, description, category, state -> set_option!(state, option, true; redisplay), state -> set_option!(state, option, false; redisplay))
end

function set_view(key::Char, option::Symbol, category, description = string(option))
    return Command(false, key, option, description, category, state -> set_option!(state, :view, option; redisplay = state.config.view !== option), nothing)
end

function set_option!(state::CthulhuState, option::Symbol, value; redisplay = false)
    (; config) = state
    hasproperty(config, option) || throw(ArgumentError("Unknown configuration option `$option`"))

    if value === true
        option === :remarks && config.optimize && @warn "Disable optimization to see the inference remarks."
        option === :inline_cost && !optimize && @warn "Enable optimization to see the inlining costs."
    end

    if option === :enable_highlighter
        value === true && @info "Using syntax highlighter $(CONFIG.highlighter)."
        value === false && @info "Turned off syntax highlighter for Julia, LLVM and native code."
    end

    if value === false
        option === :inlay_types_vscode && TypedSyntax.clear_inlay_hints_vscode()
        option === :diagnostics_vscode && TypedSyntax.clear_diagnostics_vscode()
    end

    redisplay && (state.display_code = true)
    state.config = setproperties(config, NamedTuple((option => value,)))
end

function edit_source_code(state::CthulhuState)
    def = state.mi.def
    isa(def, Method) || return @warn "Can't go to the source location because the definition is not a method."
    edit(whereis(def)...)
end

function revise_and_redisplay!(state::CthulhuState)
    # Call Revise.revise() without introducing a dependency on Revise
    id = Base.PkgId(UUID("295af30f-e4ad-537b-8983-00126c2a3abe"), "Revise")
    mod = get(Base.loaded_modules, id, nothing)
    mod === nothing && return @warn "Failed to load Revise."
    revise = getfield(mod, :revise)::Function
    revise()
    state.ci = generate_code_instance(state.provider, state.mi)
    state.display_code = true
end

function bookmark_method(state::CthulhuState)
    push!(BOOKMARKS, Bookmark(state.mi, state.provider))
    @info "The method is pushed at the end of `Cthulhu.BOOKMARKS`."
end

function dump_parameters(state::CthulhuState)
    iostream = term.out_stream::IO
    show_parameters(iostream, state.provider)
end

function split_by_category(commands::Vector{Command})
    categories = Pair{Symbol,Vector{Command}}[]
    for toggle in commands
        i = findfirst(==(toggle.category) ∘ first, categories)
        if i === nothing
            push!(categories, toggle.category => [toggle])
        else
            _, list = categories[i]
            push!(list, toggle)
        end
    end
    return categories
end

function update_default_commands!(commands::Vector{Command}, state::CthulhuState)
    (; config) = state
    for command in commands
        (; name) = command
        name === :inlay_types_vscode && set_active!(command, state, config.inlay_types_vscode)
        name === :diagnostics_vscode && set_active!(command, state, config.diagnostics_vscode)
        name === :optimize && set_active!(command, state, config.optimize)
        command.category === :show && set_active!(command, state, config.view === name)
    end
end

function set_active!(command::Command, state::CthulhuState, value::Bool)
    something(command.active, false) === value && return
    callback = ifelse(value, command.callback_on, command.callback_off)
    command.active !== nothing && (command.active = value)
    callback !== nothing && callback(state)
end

save_descend_state(state::CthulhuState) = (; state.mi, state.ci, state.override)

function restore_descend_state!(state::CthulhuState, (; mi, ci, override)::NamedTuple)
    state.mi = mi
    state.ci = ci
    state.override = override
    return state
end
