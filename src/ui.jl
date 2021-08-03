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

function show_as_line(el, optimize::Bool)
    reduced_displaysize = displaysize(stdout)::Tuple{Int,Int} .- (0, 3)
    buf = ctx = IOBuffer()
    if (color = get(stdout, :color, nothing)) !== nothing
        ctx = IOContext(ctx, :color=>color)
    end
    show(IOContext(ctx, :limit=>true, :displaysize=>reduced_displaysize, :optimize=>optimize), el)
    String(take!(buf))
end


function CthulhuMenu(callsites, optimize::Bool; pagesize::Int=10, sub_menu = false, kwargs...)
    options = vcat(map(site->show_as_line(site, optimize), callsites), ["↩"])
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

function TerminalMenus.header(m::CthulhuMenu)
    m.sub_menu && return ""
    """
    Select a call to descend into or ↩ to ascend. [q]uit. [b]ookmark.
    Toggles: [o]ptimize, [w]arn, [h]ide type-stable statements, [d]ebuginfo, [i]nlining costs, [s]yntax highlight for Source/LLVM/Native.
    Show: [S]ource code, [A]ST, [L]LVM IR, [N]ative code
    Actions: [E]dit source code, [R]evise and redisplay
    Advanced: dump [P]arams cache.
    """
end

function TerminalMenus.keypress(m::CthulhuMenu, key::UInt32)
    m.sub_menu && return false
    if key == UInt32('w')
        m.toggle = :warn
        return true
    elseif key == UInt32('h')
        m.toggle = :hide
        return true
    elseif key == UInt32('o')
        m.toggle = :optimize
        return true
    elseif key == UInt32('d')
        m.toggle = :debuginfo
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
    elseif key == UInt32('r') || key == UInt32('R')
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
