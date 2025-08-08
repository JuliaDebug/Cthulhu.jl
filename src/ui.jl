using REPL.TerminalMenus
import REPL.TerminalMenus: request
using FoldingTrees

mutable struct CthulhuMenu <: TerminalMenus.ConfiguredMenu{TerminalMenus.Config}
    options::Vector{String}
    pagesize::Int
    pageoffset::Int
    selected::Int
    toggle::Union{Nothing, Symbol}
    sub_menu::Bool
    config::TerminalMenus.Config
    commands::Vector{Command}
    state::CthulhuState
end

function show_as_line(callsite::Callsite, with_effects::Bool, exception_type::Bool, optimize::Bool, iswarn::Bool)
    reduced_displaysize = displaysize(stdout)::Tuple{Int,Int} .- (0, 3)
    sprint() do io
        show(IOContext(io,
            :limit        => true,
            :displaysize  => reduced_displaysize,
            :optimize     => optimize,
            :iswarn       => iswarn,
            :color        => iswarn | with_effects | exception_type,
            :with_effects => with_effects,
            :exception_type => exception_type),
            callsite)
    end
end

"""
    CthulhuMenu(args...; pagesize::Int=10, sub_menu = false, kwargs...)
Set up the callsite menu with the given arguments. This is largely internal,
but the main keywords to control the menu are:
- `pagesize::Int` (default 10) Number of callsites to show at a time (without scrolling).
- `sub_menu::Bool` (default false) if true, user can only pick a callsite, not change options.
Others are passed to
[`REPL.TerminalMenus.Config`](https://docs.julialang.org/en/v1/stdlib/REPL/#REPL.TerminalMenus.Config).
"""
function CthulhuMenu(state::CthulhuState, callsites, with_effects::Bool, exception_type::Bool,
                     optimize::Bool, iswarn::Bool, hide_type_stable::Bool,
                     commands::Vector{Command}; pagesize::Int=10, sub_menu = false, kwargs...)
    options = build_options(callsites, with_effects, exception_type, optimize, iswarn, hide_type_stable)
    length(options) < 1 && error("CthulhuMenu must have at least one option")

    # if pagesize is -1, use automatic paging
    pagesize = pagesize == -1 ? length(options) : pagesize
    # pagesize shouldn't be bigger than options
    pagesize = min(length(options), pagesize)
    # after other checks, pagesize must be greater than 1
    pagesize < 1 && error("pagesize must be >= 1")

    pageoffset = 0
    selected = -1 # none

    config = TerminalMenus.Config(; kwargs...)

    return CthulhuMenu(options, pagesize, pageoffset, selected, nothing, sub_menu, config, commands, state)
end

build_options(callsites::Vector{Callsite}, with_effects::Bool, exception_type::Bool, optimize::Bool, iswarn::Bool, ::Bool) =
    vcat(map(callsite->show_as_line(callsite, with_effects, exception_type, optimize, iswarn), callsites), ["↩"])
function build_options(callsites, with_effects::Bool, exception_type::Bool, optimize::Bool, iswarn::Bool, hide_type_stable::Bool)
    reduced_displaysize::Int = (displaysize(stdout)::Tuple{Int,Int})[2] - 3
    nd::Int = -1

    shown_callsites = map(callsites) do node
        if isa(node, Callsite)
            show_as_line(node, with_effects, exception_type, optimize, iswarn)
        else
            if nd == -1
                nd = TypedSyntax.ndigits_linenumbers(node)
                reduced_displaysize -= nd + 1
            end
            str = string(chomp(
                sprint(node; context=:color=>true) do io, node
                    limiter = TextWidthLimiter(io, reduced_displaysize)
                    if TypedSyntax.is_runtime(node)
                        if iswarn
                            printstyled(limiter, "runtime "; color=:red)
                        else
                            print(limiter, "runtime ")
                        end
                    end
                    printstyled(limiter, node; iswarn, hide_type_stable, with_linenumber=false)
                end))
            replace(str, r"\n *" => s" ")  # in case of multiline code, issue #428
        end
    end
    push!(shown_callsites, "↩")
    return shown_callsites
end

TerminalMenus.options(m::CthulhuMenu) = m.options
TerminalMenus.cancel(m::CthulhuMenu) = m.selected = -1

stringify(@nospecialize(f), io::IO = devnull) = stringify(f, IOContext(io, :color=>true))
function stringify(@nospecialize(f), context::IOContext)
    buf = IOBuffer()
    io = IOContext(buf, context)
    f(io)
    return String(take!(buf))
end

const debugcolors = (:nothing, :light_black, :yellow)
function usage(config, commands::Vector{Command})
    colorize(use_color::Bool, c::Char) = stringify() do io
        use_color ? printstyled(io, c; color=:cyan) : print(io, c)
    end

    buffer = IOBuffer()
    io = IOContext(buffer, :color => true)

    println(io, "\rSelect a call to descend into or ↩ to ascend.")
    # XXX: Add multi-color `debuginfo` printing (see `debugcolors`)
    categories = split_by_category(commands)
    for (category, list) in categories
        print(io, '\n', uppercasefirst(string(category)), ": ")
        is_first = true
        for command in list
            (; description) = command

            description == "vscode: inlay types" && !TypedSyntax.inlay_hints_available_vscode() && continue
            description == "vscode: diagnostics" && !TypedSyntax.diagnostics_available_vscode() && continue
            config.view === :source && in(description, ("optimize", "debuginfo", "remarks", "effects", "exception types", "inlining costs")) && continue

            !is_first && print(io, ", ")
            is_first = false

            key = Char(command.key)
            i = findfirst(x -> lowercase(x) == lowercase(key), description)
            if i === nothing
                i = 1
                description = "$key: $description"
            end
            print(description[1:prevind(description, i)])
            print(io, '[', colorize(something(command.active, false), key), ']')
            print(io, description[nextind(description, i):end])
        end
        print(io, '.')
    end
    println(io)
    return String(take!(buffer))
end

function TerminalMenus.keypress(menu::CthulhuMenu, key::UInt32)
    menu.sub_menu && return false
    i = findfirst(x -> x.key == key, menu.commands)
    i === nothing && return false
    command = menu.commands[i]
    set_active!(command, menu.state, !something(command.active, false))
    menu.toggle = command.name
    return true
end

function TerminalMenus.pick(menu::CthulhuMenu, cursor::Int)
    menu.selected = cursor
    return true # break out of the menu
end

function TerminalMenus.writeline(buffer::IOBuffer, menu::CthulhuMenu, idx::Int, iscursor::Bool)
    line = replace(menu.options[idx], "\n" => "\\n")
    print(buffer, line)
end
