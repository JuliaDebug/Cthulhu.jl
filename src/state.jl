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
                      ci = nothing, mi = ci === nothing ? nothing : get_mi(ci), override = nothing)
    return CthulhuState(terminal, provider, config, mi, ci, override, true)
end

function default_terminal()
    term_env = get(ENV, "TERM", @static Sys.iswindows() ? "" : "dumb")
    term = REPL.Terminals.TTYTerminal(term_env, stdin, stdout, stderr)
    return term
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
    f::Any
    key::UInt32
    name::Symbol
    description::String
    category::Symbol
    function Command(@nospecialize(f), key, name, description, category)
        key = convert(UInt32, key)
        description = convert(String, description)
        return new(f, key, name, description, category)
    end
end

function default_menu_commands(provider::AbstractProvider)
    commands = Command[
        set_option('w', :iswarn, :toggles, "warn"),
        set_option('h', :hide_type_stable, :toggles, "hide type-stable statements"),
        set_option('o', :optimize, :toggles),
        set_debuginfo('d'),
        set_option('r', :remarks, :toggles),
        set_option('e', :effects, :toggles, "effects"),
        set_option('x', :exception_types, :toggles),
        set_option('i', :inlining_costs, :toggles, "inlining costs"),
        set_option('t', :type_annotations, :toggles),
        set_option('s', :enable_highlighter, :toggles, "syntax highlight for Source/LLVM/Native"),
        set_option('v', :inlay_types_vscode, :toggles, "vscode: inlay types"),
        set_option('V', :diagnostics_vscode, :toggles, "vscode: diagnostics"),
        set_option('j', :jump_always, :toggles, "jump to source always"; redisplay = false),
        set_view('S', :source, :show),
        set_view('A', :ast, :show, "AST"),
        set_view('T', :typed, :show),
        set_view('L', :llvm, :show, "LLVM"),
        set_view('N', :native, :show),
        perform_action(_ -> nothing, 'q', :quit, :actions), # built-in from TerminalMenus
        perform_action(_ -> nothing, '⟵', :ascend, :actions),
        perform_action(bookmark_method, 'b', :bookmark, :actions),
        perform_action(edit_source_code, 'E', :edit, :actions, "Edit source code"),
        perform_action(revise_and_redisplay!, 'R', :revise, :actions, "Revise and redisplay"),
        perform_action(dump_parameters, 'P', :dump_params, :actions, "dump params cache"),
    ]
    return commands
end

function perform_action(f, key::Char, name::Symbol, category, description = string(name))
    return Command(f, key, name, description, category)
end

function set_debuginfo(key::Char)
    return Command(key, :debuginfo, "debuginfo", :toggles) do state
        values = (:none, :compact, :source)
        previous = state.config.debuginfo
        i = findfirst(==(previous), values)
        next = i === nothing ? :none : values[mod1(i + 1, 3)]
        return set_option!(state, :debuginfo, next; redisplay = true)
    end
end

function set_option(key::Char, option::Symbol, category, description = string(option); redisplay = true)
    return Command(state -> toggle_option!(state, option; redisplay), key, option, description, category)
end

function set_view(key::Char, option::Symbol, category, description = string(option))
    return Command(state -> set_option!(state, :view, option; redisplay = state.config.view !== option), key, option, description, category)
end

function toggle_option(state::CthulhuState, option::Symbol)
    (; config) = state
    hasproperty(config, option) || throw(ArgumentError("Unknown configuration option `$option`"))
    previous = getproperty(config, option)
    return !previous
end

function toggle_option!(state::CthulhuState, option::Symbol; redisplay = false)
    value = toggle_option(state, option)
    set_option!(state, option, value; redisplay)
end

function set_option!(state::CthulhuState, option::Symbol, value; redisplay = false)
    (; config) = state
    hasproperty(config, option) || throw(ArgumentError("Unknown configuration option `$option`"))

    if value === true
        if option === :remarks && config.optimize || option === :optimize && config.remarks
            @warn "Disable optimization to see the inference remarks."
        end
        option === :inlining_costs && !config.optimize && @warn "Enable optimization to see the inlining costs."
    end

    if option === :enable_highlighter
        value === true && @info "Using syntax highlighter $(CONFIG.highlighter)."
        value === false && @info "Turned off syntax highlighter for Julia, LLVM and native code."
    end

    if value === false
        option === :optimize && config.inlining_costs && @warn "Enable optimization to see the inlining costs."
        option === :inlay_types_vscode && TypedSyntax.clear_inlay_hints_vscode()
        option === :diagnostics_vscode && TypedSyntax.clear_diagnostics_vscode()
    end

    redisplay && (state.display_code = true)
    state.config = set_config(config, NamedTuple((option => value,)))
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
    mod === nothing && return @warn "Revise must be loaded to revise changes"
    revise = getfield(mod, :revise)::Function
    revise()
    world = Base.get_world_counter()
    state.mi = find_method_instance(state.provider, state.mi.specTypes, world)
    state.ci = generate_code_instance(state.provider, state.mi)
    state.display_code = true
end

function bookmark_method(state::CthulhuState)
    push!(BOOKMARKS, Bookmark(state.provider, state.ci; state.config))
    mod = resolve_module(state.provider)
    @info "The current `descend` state was saved for later use. You may access it with `$mod.BOOKMARKS[end]`."
    if nameof(mod) === :CthulhuCompilerExt
        @info "You can get the `CthulhuCompilerExt` module with `Base.get_extension(Cthulhu, :CthulhuCompilerExt)`"
    end
end

function dump_parameters(state::CthulhuState)
    io = state.terminal.out_stream::IO
    show_parameters(io, state.provider)
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

save_descend_state(state::CthulhuState) = (; state.mi, state.ci, state.override)

function restore_descend_state!(state::CthulhuState, (; mi, ci, override)::NamedTuple)
    state.mi = mi
    state.ci = ci
    state.override = override
    return state
end

# AbstractProvider interface

menu_commands(provider::AbstractProvider) = default_menu_commands(provider)

is_command_enabled(provider::AbstractProvider, state::CthulhuState, command::Command) =
    is_default_command_enabled(provider, state, command)

function is_default_command_enabled(provider::AbstractProvider, state::CthulhuState, command::Command)
    command.name === :inlay_types_vscode && return TypedSyntax.inlay_hints_available_vscode()
    command.name === :diagnostics_vscode && return TypedSyntax.diagnostics_available_vscode()
    if state.config.view === :source
        disabled_commands = (:optimize, :debuginfo, :remarks, :effects, :exception_types, :inlining_costs)
        return !in(command.name, disabled_commands)
    end
    return true
end

function show_command(io::IO, provider::AbstractProvider, state::CthulhuState, command::Command)
    (; description) = command
    key = Char(command.key)
    style = style_for_command_key(provider, state, command)
    i = findfirst(x -> lowercase(x) == lowercase(key), description)
    if i === nothing
        i = 1
        printstyled(io, key; style...)
        print(io, ' ', description)
        return
    end
    print(io, description[1:prevind(description, i)])
    print(io, '[')
    printstyled(io, key; style...)
    print(io, ']')
    print(io, description[nextind(description, i):end])
end

style_for_command_key(provider::AbstractProvider, state::CthulhuState, command::Command) =
    default_style_for_command_key(provider, state, command)

function default_style_for_command_key(provider::AbstractProvider, state::CthulhuState, command::Command)
    value = value_for_command(provider, state, command)
    isa(value, Symbol) && command.name === :debuginfo && return debuginfo_style(value)
    return default_style(value)
end

value_for_command(provider::AbstractProvider, state::CthulhuState, command::Command) =
    value_for_default_command(provider, state, command)

function value_for_default_command(provider::AbstractProvider, state::CthulhuState, command::Command)
    command.category === :show && return state.config.view === command.name
    command.category !== :toggles && return nothing
    !hasproperty(state.config, command.name) && return nothing
    return getproperty(state.config, command.name)
end

default_style(@nospecialize(value)) = NamedTuple()
default_style(value::Nothing) = (; color=:cyan, bold=false)
function default_style(value::Bool)
    color = ifelse(value, :green, :red)
    return (; color, bold = value)
end

function debuginfo_style(value::Symbol)
    i = findfirst(==(value), (:none, :compact, :source))
    i === nothing && return NamedTuple()
    color = (:red, :light_black, :green)[i]
    return (; color, bold = color === :green)
end
