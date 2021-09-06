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
end

function show_as_line(callsite::Callsite, optimize::Bool, iswarn::Bool)
    reduced_displaysize = displaysize(stdout)::Tuple{Int,Int} .- (0, 3)
    sprint() do io
        show(IOContext(io, :limit=>true, :displaysize=>reduced_displaysize, :optimize=>optimize, :iswarn=>iswarn, :color=>iswarn), callsite)
    end
end

function CthulhuMenu(callsites, optimize::Bool, iswarn::Bool; pagesize::Int=10, sub_menu = false, kwargs...)
    options = vcat(map(callsite->show_as_line(callsite, optimize, iswarn), callsites), ["↩"])
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
    CthulhuMenu(options, pagesize, pageoffset, selected, nothing, sub_menu, config)
end

TerminalMenus.options(m::CthulhuMenu) = m.options
TerminalMenus.cancel(m::CthulhuMenu) = m.selected = -1

function stringify(@nospecialize(f), io::IO=IOBuffer())
    f(IOContext(io, :color=>true))
    return String(take!(io))
end

const debugcolors = (:nothing, :light_black, :yellow)
function usage(@nospecialize(view_cmd), optimize, iswarn, hide_type_stable, debuginfo, remarks, inline_cost, highlight)
    colorize(iotmp, use_color::Bool, c::Char) = stringify(iotmp) do io
        use_color ? printstyled(io, c; color=:cyan) : print(io, c)
    end

    io, iotmp = IOBuffer(), IOBuffer()
    ioctx = IOContext(io, :color=>true)

    println(ioctx, "Select a call to descend into or ↩ to ascend. [q]uit. [b]ookmark.")
    println(ioctx, "Toggles: [",
        colorize(iotmp, optimize, 'o'), "]ptimize, [",
        colorize(iotmp, iswarn, 'w'), "]arn, [",
        colorize(iotmp, hide_type_stable, 'h'), "]ide type-stable statements, [",
        stringify(iotmp) do io
            printstyled(io, 'd'; color=debugcolors[Int(debuginfo)+1])
        end, "]ebuginfo, [",
        colorize(iotmp, remarks, 'r'), "]emarks, [",
        colorize(iotmp, inline_cost, 'i'), "]nlining costs, [",
        colorize(iotmp, highlight, 's'), "]yntax highlight for Source/LLVM/Native.")
    println(ioctx, "Show: [",
        colorize(iotmp, view_cmd === cthulhu_source, 'S'), "]ource code, [",
        colorize(iotmp, view_cmd === cthulhu_ast, 'A'), "]ST, [",
        colorize(iotmp, view_cmd === cthulhu_typed, 'T'), "]yped code, [",
        colorize(iotmp, view_cmd === cthulhu_llvm, 'L'), "]LVM IR, [",
        colorize(iotmp, view_cmd === cthulhu_native, 'N'), "]ative code")
    print(ioctx,
    """
    Actions: [E]dit source code, [R]evise and redisplay
    Advanced: dump [P]arams cache.""")
    return String(take!(io))
end

function TerminalMenus.keypress(m::CthulhuMenu, key::UInt32)
    m.sub_menu && return false
    if key == UInt32('w')
        m.toggle = :warn
        return true
    elseif key == UInt32('h')
        m.toggle = :hide_type_stable
        return true
    elseif key == UInt32('o')
        m.toggle = :optimize
        return true
    elseif key == UInt32('d')
        m.toggle = :debuginfo
        return true
    elseif key == UInt32('r')
        m.toggle = :remarks
        return true
    elseif key == UInt32('i')
        m.toggle = :inline_cost
        return true
    elseif key == UInt32('s')
        m.toggle = :highlighter
        return true
    elseif key == UInt32('S')
        m.toggle = :source
        return true
    elseif key == UInt32('A')
        m.toggle = :ast
        return true
    elseif key == UInt32('T')
        m.toggle = :typed
        return true
    elseif key == UInt32('L')
        m.toggle = :llvm
        return true
    elseif key == UInt32('N')
        m.toggle = :native
        return true
    elseif key == UInt32('P')
        m.toggle = :dump_params
        return true
    elseif key == UInt32('b')
        m.toggle = :bookmark
        return true
    elseif key == UInt32('R')
        m.toggle = :revise
        return true
    elseif key == UInt32('e') || key == UInt32('E')
        m.toggle = :edit
        return true
    end
    return false
end

function TerminalMenus.pick(menu::CthulhuMenu, cursor::Int)
    menu.selected = cursor
    return true #break out of the menu
end

function TerminalMenus.writeline(buf::IOBuffer, menu::CthulhuMenu, idx::Int, iscursor::Bool)
    line = replace(menu.options[idx], "\n" => "\\n")
    print(buf, line)
end
