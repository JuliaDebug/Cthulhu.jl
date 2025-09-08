descend_impl(@nospecialize(args...); kwargs...) =
    descend_with_error_handling(args...; iswarn=true, kwargs...)

function descend_with_error_handling(args...; terminal=default_terminal(), kwargs...)
    @nospecialize
    try
        _descend(terminal, args...; kwargs...)
    catch x
        TypedSyntax.clear_all_vscode()
        if x isa InterruptException
            return nothing
        else
            rethrow(x)
        end
    end
    return nothing
end

function default_terminal()
    term_env = get(ENV, "TERM", @static Sys.iswindows() ? "" : "dumb")
    term = REPL.Terminals.TTYTerminal(term_env, stdin, stdout, stderr)
    return term
end

##
# descend! is the main driver function.
# src/reflection.jl has the tools to discover methods.
# src/state.jl provides the state that is mutated by `descend!`.
# src/ui.jl provides the user facing interface to which `descend!` responds.
##
function descend!(state::CthulhuState)
    (; provider) = state
    commands = menu_commands(provider)
    if !isa(commands, Vector{Command})
        error(lazy"""
        invalid `$AbstractProvider` API:
        `$(Cthulhu.commands)(provider::$(typeof(provider))` is expected to return a `Vector{Command}` object.
        """)
    end
    while true
        (; config, mi, ci) = state
        iostream = state.terminal.out_stream::IO
        menu_options = (; cursor = '•', scroll_wrap = true, config.menu_options...)

        mi::MethodInstance, ci::CodeInstance

        src = something(state.override, ci)
        result = LookupResult(provider, src, config.optimize)
        if result === nothing
            if should_regenerate_code_instance(provider, src)
                additional_descend(src)
                break
            end
        end

        if config.jump_always
            def = state.mi.def
            if isa(def, Method)
                @warn "Can't jump to source location because the definition is not a method."
            else
                if isdefined(Main, :VSCodeServer) && Main.VSCodeServer isa Module && isdefined(Main.VSCodeServer, :openfile)
                    Main.VSCodeServer.openfile(whereis(def)...; preserve_focus=true)
                else
                    edit(whereis(def)...)
                end
            end
        end

        if state.display_code
            printstyled(IOContext(iostream, :limit => true), mi.def, '\n'; bold=true)
            ioctx = IOContext(iostream,
                        :color => true,
                        :displaysize => displaysize(iostream), # displaysize doesn't propagate otherwise
                        :SOURCE_SLOTNAMES => source_slotnames(result),
                        :effects => config.effects,
                        :exception_type => config.exception_type)
            str = stringify(ioctx) do io
                view_function(state)(io, provider, state, result)
            end
            # eliminate trailing indentation (see first item in bullet list in PR #189)
            rmatch = findfirst(r"\u001B\[90m\u001B\[(\d+)G( *)\u001B\[1G\u001B\[39m\u001B\[90m( *)\u001B\[39m$", str)
            if rmatch !== nothing
                str = str[begin:prevind(str, first(rmatch))]
            end
            print(iostream, str)
        end

        callsites, source_nodes = find_callsites(provider, result, ci, config.view === :source)

        @label show_menu

        shown_callsites = config.view === :source ? source_nodes : callsites

        state.display_code = false
        menu = CthulhuMenu(state, shown_callsites, config.effects, config.exception_type, config.optimize,
                           config.iswarn & get(iostream, :color, false)::Bool,
                           config.hide_type_stable, commands; menu_options...)
        usg = usage(provider, state, commands)
        cid = request(state.terminal, usg, menu)
        toggle = menu.toggle
        toggle === :ascend && break

        if toggle === nothing
            callsite = select_callsite(state, callsites, source_nodes, cid, menu_options, commands)::Union{Symbol, Callsite}
            callsite === :ascend && break
            callsite === :interrupted && break
            callsite === :failed && continue
            callsite === :continue && continue
            (; info) = callsite

            # forcibly enter and inspect the frame, although the native interpreter gave up
            if info isa TaskCallInfo
                @info """
                Inference didn't analyze this call because it is a dynamic call:
                Cthulhu nevertheless is trying to descend into it for further inspection.
                """
                state.ci = get_ci(info)::CodeInstance
                descend!(state)
                continue
            elseif info isa RTCallInfo
                @info """
                This is a runtime call. You cannot descend into it.
                """
                @goto show_menu
            end

            ci = get_ci(callsite)
            ci === nothing && continue

            # Recurse into the callsite.

            prev = save_descend_state(state)
            state.mi = get_mi(ci)
            state.ci = ci
            state.override = get_override(provider, info)
            state.display_code = true
            descend!(state)
            restore_descend_state!(state, prev)
            state.display_code = true

            println(iostream)
        end
    end

    TypedSyntax.clear_all_vscode()
end

function select_callsite(state::CthulhuState, callsites::Vector{Callsite}, source_nodes, i::Int, menu_options::NamedTuple, commands::Vector{Command})
    (; config) = state
    i === length(callsites) + 1 && return :ascend
    if i === -1
        config.interruptexc && throw(InterruptException())
        return :interrupted
    end

    callsite = callsites[i]
    !isa(callsite.info, MultiCallInfo) && return callsite

    (; info) = callsite
    source_node = !isempty(source_nodes) ? source_nodes[i] : nothing
    sub_callsites = map(ci -> Callsite(callsite.id, ci, callsite.head), info.callinfos)
    if isempty(sub_callsites)
        @eval Main begin
            provider = $provider
            mi = $mi
            info = $info
        end
        @error "Expected multiple callsites, but found none. Please fill an issue with a reproducing example."
        return :failed
    end
    menu_callsites = source_node === nothing ? sub_callsites : menu_callsites_from_source_node(callsite, source_node)
    io = state.terminal.out_stream::IO
    printstyled(io, "\nChoose which of the possible calls to descend into:"; color=:blue)
    menu = CthulhuMenu(state, menu_callsites, config.effects, config.exception_type, config.optimize,
                        false, false, commands;
                        sub_menu=true, menu_options...)
    i = request(state.terminal, "", menu)
    i === length(sub_callsites) + 1 && return :continue
    if i === -1
        config.interruptexc && throw(InterruptException())
        return :interrupted
    end

    return sub_callsites[i]
end

function menu_callsites_from_source_node(callsite::Callsite, source_node)
    callsites = Union{Callsite,TypedSyntax.MaybeTypedSyntaxNode}[]
    for info in callsite.info.callinfos
        ci = get_ci(info)
        mi = get_mi(ci)
        p = Base.unwrap_unionall(mi.specTypes).parameters
        if isa(source_node, TypedSyntax.MaybeTypedSyntaxNode) && length(p) == length(JuliaSyntax.children(source_node)) + 1
            new_node = copy(source_node)
            for (i, child) in enumerate(JuliaSyntax.children(new_node))
                child.typ = p[i+1]
            end
            push!(callsites, new_node)
        else
            push!(callsites, Callsite(callsite.id, info, callsite.head))
        end
    end
    return callsites
end

function source_slotnames(result::LookupResult)
    result.src === nothing && return false
    return Base.sourceinfo_slotnames(result.src)
end
