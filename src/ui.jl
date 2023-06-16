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
    custom_toggles::Vector{CustomToggle}
end

function show_as_line(callsite::Callsite, with_effects::Bool, optimize::Bool, iswarn::Bool)
    reduced_displaysize = displaysize(stdout)::Tuple{Int,Int} .- (0, 3)
    sprint() do io
        show(IOContext(io,
            :limit        => true,
            :displaysize  => reduced_displaysize,
            :optimize     => optimize,
            :iswarn       => iswarn,
            :color        => iswarn | with_effects,
            :with_effects => with_effects),
            callsite)
    end
end

function CthulhuMenu(callsites, with_effects::Bool, optimize::Bool, iswarn::Bool, hide_type_stable::Bool,
                     custom_toggles::Vector{CustomToggle}; pagesize::Int=10, sub_menu = false, kwargs...)
    options = build_options(callsites, with_effects, optimize, iswarn, hide_type_stable)
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

    return CthulhuMenu(options, pagesize, pageoffset, selected, nothing, sub_menu, config, custom_toggles)
end

build_options(callsites::Vector{Callsite}, with_effects::Bool, optimize::Bool, iswarn::Bool, ::Bool) =
    vcat(map(callsite->show_as_line(callsite, with_effects, optimize, iswarn), callsites), ["↩"])
function build_options(callsites, with_effects::Bool, optimize::Bool, iswarn::Bool, hide_type_stable::Bool)
    reduced_displaysize::Int = (displaysize(stdout)::Tuple{Int,Int})[2] - 3
    nd::Int = -1

    shown_callsites = map(callsites) do node
        if isa(node, Callsite)
            show_as_line(node, with_effects, optimize, iswarn)
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
                    printstyled(limiter, node; iswarn, hide_type_stable, with_linenumber=false, vscode_integration=false)
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
function usage(@nospecialize(view_cmd), annotate_source, optimize, iswarn, hide_type_stable, debuginfo, remarks, with_effects, inline_cost, type_annotations, highlight, hide_inlay_types_vscode, hide_warn_diagnostics_vscode,
    custom_toggles::Vector{CustomToggle})
    colorize(use_color::Bool, c::Char) = stringify() do io
        use_color ? printstyled(io, c; color=:cyan) : print(io, c)
    end

    io = IOBuffer()
    ioctx = IOContext(io, :color=>true)

    println(ioctx, "Select a call to descend into or ↩ to ascend. [q]uit. [b]ookmark.")
    print(ioctx, "Toggles: [",
        colorize(iswarn, 'w'), "]arn, [",
        colorize(hide_type_stable, 'h'), "]ide type-stable statements, [",
        colorize(type_annotations, 't'), "]ype annotations, [",
        colorize(highlight, 's'), "]yntax highlight for Source/LLVM/Native")
    if TypedSyntax.inlay_hints_available()
        print(ioctx, ", [",
        colorize(hide_inlay_types_vscode, 'v'), "]scode: hide inlay types")
    end
    if TypedSyntax.isvscode()
        print(ioctx, ", [",
        colorize(hide_warn_diagnostics_vscode, 'V'), "]scode: hide warn diagnostics")
    end
    if !annotate_source
        print(ioctx, ", [",
            colorize(optimize, 'o'), "]ptimize, [",
            stringify() do io
                printstyled(io, 'd'; color=debugcolors[Int(debuginfo)+1])
            end, "]ebuginfo, [",
            colorize(remarks, 'r'), "]emarks, [",
            colorize(with_effects, 'e'), "]ffects, [",
            colorize(inline_cost, 'i'), "]nlining costs")
    end
    for i = 1:length(custom_toggles)
        ct = custom_toggles[i]
        print(ioctx, ", [", colorize(ct.onoff, Char(ct.key)), ']', ct.description)
        i ≠ length(custom_toggles) && print(ioctx, ", ")
    end
    print(ioctx, '.')
    println(ioctx)
    println(ioctx, "Show: [",
        colorize(annotate_source, 'S'), "]ource code, [",
        colorize(view_cmd === cthulhu_ast, 'A'), "]ST, [",
        colorize(!annotate_source && view_cmd === cthulhu_typed, 'T'), "]yped code, [",
        colorize(view_cmd === cthulhu_llvm, 'L'), "]LVM IR, [",
        colorize(view_cmd === cthulhu_native, 'N'), "]ative code")
    print(ioctx,
    """
    Actions: [E]dit source code, [R]evise and redisplay""")
    if !annotate_source
        print(ioctx,
        """
        \nAdvanced: dump [P]arams cache.""")
    end
    return String(take!(io))
end

const TOGGLES = Dict(
    UInt32('w') => :warn,
    UInt32('h') => :hide_type_stable,
    UInt32('o') => :optimize,
    UInt32('d') => :debuginfo,
    UInt32('r') => :remarks,
    UInt32('e') => :with_effects,
    UInt32('i') => :inline_cost,
    UInt32('t') => :type_annotations,
    UInt32('s') => :highlighter,
    UInt32('S') => :source,
    UInt32('A') => :ast,
    UInt32('T') => :typed,
    UInt32('L') => :llvm,
    UInt32('N') => :native,
    UInt32('P') => :dump_params,
    UInt32('b') => :bookmark,
    UInt32('R') => :revise,
    UInt32('E') => :edit,
    UInt32('v') => :hide_inlay_types_vscode,
    UInt32('V') => :hide_warn_diagnostics_vscode
)

function TerminalMenus.keypress(m::CthulhuMenu, key::UInt32)
    m.sub_menu && return false
    toggle = get(TOGGLES, key, nothing)
    if !isnothing(toggle)
        m.toggle = toggle
        return true
    end
    local i = findfirst(ct->ct.key == key, m.custom_toggles)
    isnothing(i) && return false
    m.toggle = m.custom_toggles[i].toggle
    return true
end

function TerminalMenus.pick(menu::CthulhuMenu, cursor::Int)
    menu.selected = cursor
    return true #break out of the menu
end

function TerminalMenus.writeline(buf::IOBuffer, menu::CthulhuMenu, idx::Int, iscursor::Bool)
    line = replace(menu.options[idx], "\n" => "\\n")
    print(buf, line)
end
