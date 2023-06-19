isvscode() = isdefined(Main, :VSCodeServer) && Main.VSCodeServer isa Module
inlay_hints_available() = isvscode() && isdefined(Main.VSCodeServer, :INLAY_HINTS_ENABLED)

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
function add_diagnostic!(warn_diagnostics, node, position, severity)
    file_path = node.filename
    line, column = source_location(node, position)
    push!(warn_diagnostics, WarnUnstable(file_path, line, severity))
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
function Base.show(io::IO, ::MIME"application/vnd.julia-vscode.inlayHints", type_hints_by_file::Dict{T, <:AbstractVector{InlayHint}}) where T<:AbstractString
    if inlay_hints_available()
        return Dict{T, Vector{NamedTuple{(:position, :label, :kind), Tuple{Tuple{Int, Int}, String, Union{Nothing, Int}}}}}(filepath => map(x -> (position=(x.line, x.column), label=x.label, kind=x.kind), type_hints) for (filepath, type_hints) in type_hints_by_file)
    end
    return nothing
end
function add_hint!(type_hints, message, node, position; kind=InlayHintKinds.Type)
    filepath = node.filename
    line, column = source_location(node, position)

    if filepath ∉ keys(type_hints)
        type_hints[filepath] = InlayHint[]
    end
    push!(type_hints[filepath], InlayHint(line-1, column, message, kind))
end

function show_annotation(io, @nospecialize(T), post, node, position, type_hints, warn_diagnostics; iswarn::Bool)
    print(io, post)
    
    T_str = string(T)
    if iswarn && is_type_unstable(T)
            printstyled(io, "::", T_str; color=is_small_union_or_tunion(T) ? :yellow : :red)
            add_diagnostic!(warn_diagnostics, node, position, is_small_union_or_tunion(T) ? 2 : 1)
            add_hint!(type_hints, string(post, "::", T_str), node, position; kind=nothing)
    else        
        printstyled(io, "::", T_str; color=:cyan)
        add_hint!(type_hints, string(post, "::", T_str), node, position; kind=InlayHintKinds.Type)
    end
end

function _print(io::IO, x, node, position, type_hints)
    _print(io, x, node, position, nothing)
    
    if !isempty(x) && position > 0 # position > 0 hacky fix not sure what the actual bug is
        add_hint!(type_hints, x, node, position)
    end
end