module VSCodeExt

using TypedSyntax: gettyp, ndigits_linenumbers, get_function_def, first_byte, type_annotation_mode, catchup, show_annotation, last_byte, is_function_def, children, MaybeTypedSyntaxNode, haschildren, source_line, source_location, is_type_unstable, is_small_union_or_tunion, _printstyled, WarnUnstable, InlayHint
import TypedSyntax: show_annotation, _print

function Base.show(io::IO, ::MIME"application/vnd.julia-vscode.diagnostics", warn_diagnostics::AbstractVector{WarnUnstable})
    return (
        source = "Cthulhu",
        items = map(warn_diagnostics) do warn_info
            return (; msg = "Unstable Type",
                path = warn_info.path,
                range = warn_info.range,
                severity = warn_info.severity
            )
        end,
    )
end

const InlayHintKinds = (
    Type = 1,
    Parameter = 2
)
if isdefined(Main, :VSCodeServer) && Main.VSCodeServer isa Module && isdefined(Main.VSCodeServer, :INLAY_HINTS_ENABLED)
    function Base.show(io::IO, ::MIME"application/vnd.julia-vscode.inlayHints", type_hints_by_file::Dict{String, Vector{InlayHint}})
        return Dict(filepath => map(x -> (position=(x.line, x.column), label=x.label, kind=x.kind), type_hints) for (filepath, type_hints) in type_hints_by_file)
    end
end
function add_hint!(type_hints, message, source_node, position; kind=InlayHintKinds.Type)
    filepath = source_node.filename
    line, column = source_location(source_node, position)

    if filepath ∉ keys(type_hints)
        type_hints[filepath] = InlayHint[]
    end
    push!(type_hints[filepath], InlayHint(line-1, column, message, kind))
end

function show_annotation(io, @nospecialize(T), post, source_node, position, type_hints, warn_diagnostics; iswarn::Bool)
    print(io, post)
    if iswarn && is_type_unstable(T)
        color = is_small_union_or_tunion(T) ? :yellow : :red
        printstyled(io, "::", T; color)

        file_path = source_node.filename
        line, column = source_location(source_node, position)
        push!(warn_diagnostics, WarnUnstable(file_path, ((line, column-1), (line, column)), is_small_union_or_tunion(T) ? 2 : 1))
        add_hint!(type_hints, string(post, "::", T), source_node, position; kind=nothing)
    else
        printstyled(io, "::", T; color=:cyan)
        
        add_hint!(type_hints, string(post, "::", T), source_node, position; kind=InlayHintKinds.Type)
    end
end

function _print(io::IO, x, source_node, position, type_hints)
    print(io, x)
    if !isempty(x)
        add_hint!(type_hints, x, source_node, position)
    end
end
end