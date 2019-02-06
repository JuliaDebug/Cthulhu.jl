import TerminalMenus
import TerminalMenus: request

mutable struct CthulhuMenu <: TerminalMenus.AbstractMenu
    options::Vector{String}
    pagesize::Int
    pageoffset::Int
    selected::Int
    toggle::Union{Nothing, Symbol}
end

function show_as_line(el)
    reduced_displaysize = displaysize(stdout) .- (0, 3)
    buf = IOBuffer()
    show(IOContext(buf, :limit=>true, :displaysize=>reduced_displaysize), el)
    String(take!(buf))
end


function CthulhuMenu(callsites; pagesize::Int=10)
    options = vcat(map(show_as_line, callsites), ["↩"])
    length(options) < 1 && error("CthulhuMenu must have at least one option")

    # if pagesize is -1, use automatic paging
    pagesize = pagesize == -1 ? length(options) : pagesize
    # pagesize shouldn't be bigger than options
    pagesize = min(length(options), pagesize)
    # after other checks, pagesize must be greater than 1
    pagesize < 1 && error("pagesize must be >= 1")

    pageoffset = 0
    selected = -1 # none

    CthulhuMenu(options, pagesize, pageoffset, selected, nothing)
end

TerminalMenus.options(m::CthulhuMenu) = m.options
TerminalMenus.cancel(m::CthulhuMenu) = m.selected = -1

function TerminalMenus.header(m::CthulhuMenu)
    """
    Select a call to descend into or ↩ to ascend. [q]uit.
    Toggles: [o]ptimize, [w]arn, [d]ebuginfo.
    Display: [L] for code_llvm, [N] for code_native
    """
end

function TerminalMenus.keypress(m::CthulhuMenu, key::UInt32)
    if key == UInt32('w')
        m.toggle = :warn
        return true
    elseif key == UInt32('o')
        m.toggle = :optimize
        return true
    elseif key == UInt32('d')
        m.toggle = :debuginfo
        return true
    elseif key == UInt32('L')
        m.toggle = :llvm
        return true
    elseif key == UInt32('N')
        m.toggle = :native
        return true
    end
    return false
end

function TerminalMenus.pick(menu::CthulhuMenu, cursor::Int)
    menu.selected = cursor
    return true #break out of the menu
end

function TerminalMenus.writeLine(buf::IOBuffer, menu::CthulhuMenu, idx::Int, cursor::Bool, term_width::Int)
    cursor_len = length(TerminalMenus.CONFIG[:cursor])
    # print a ">" on the selected entry
    cursor ? print(buf, TerminalMenus.CONFIG[:cursor]) : print(buf, repeat(" ", cursor_len))
    print(buf, " ") # Space between cursor and text

    line = replace(menu.options[idx], "\n" => "\\n")
    line = TerminalMenus.trimWidth(line, term_width, !cursor, cursor_len)

    print(buf, line)
end

