isvscode() = isdefined(Main, :VSCodeServer) && Main.VSCodeServer isa Module

struct WarnUnstable
    path::String
    line::Int
    severity::Int # 0: Error, 1: Warning, 2: Information, 3: Hint
end
function Base.show(io::IO, ::MIME"application/vnd.julia-vscode.diagnostics", warn_diagnostics::AbstractVector{WarnUnstable})
    return (
        source = "Cthulhu",
        items = map(warn_diagnostics) do warn_info
            return (; msg = "Unstable Type",
                path = warn_info.path,
                line = warn_info.line,
                severity = warn_info.severity
            )
        end,
    )
end

const InlayHintKinds = (
    Type = 1,
    Parameter = 2
)
struct InlayHint
    line::Int
    column::Int
    label::String
    kind::Union{Nothing, Int}
end
function Base.show(io::IO, ::MIME"application/vnd.julia-vscode.inlayHints", type_hints_by_file::Dict{String, Vector{InlayHint}})
    if isvscode() && isdefined(Main.VSCodeServer, :INLAY_HINTS_ENABLED)
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
    show_annotation(io, T, post, source_node, position, nothing, nothing; iswarn)

    if iswarn && is_type_unstable(T)
        file_path = source_node.filename
        line, column = source_location(source_node, position)
        push!(warn_diagnostics, WarnUnstable(file_path, line, is_small_union_or_tunion(T) ? 2 : 1))
        add_hint!(type_hints, string(post, "::", T), source_node, position; kind=nothing)
    else        
        add_hint!(type_hints, string(post, "::", T), source_node, position; kind=InlayHintKinds.Type)
    end
end

function _print(io::IO, x, source_node, position, type_hints)
    _print(io, x, source_node, position, nothing)
    
    if !isempty(x) && position > 0 # position > 0 hacky fix not sure what the actual bug is
        add_hint!(type_hints, x, source_node, position)
    end
end