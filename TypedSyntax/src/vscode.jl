diagnostics_available_vscode() = isdefined(Main, :VSCodeServer) && Main.VSCodeServer isa Module && isdefined(Main.VSCodeServer, :DIAGNOSTICS_ENABLED) && Main.VSCodeServer.DIAGNOSTICS_ENABLED[]
inlay_hints_available_vscode() = isdefined(Main, :VSCodeServer) && Main.VSCodeServer isa Module && isdefined(Main.VSCodeServer, :INLAY_HINTS_ENABLED) && Main.VSCodeServer.INLAY_HINTS_ENABLED[]

module DiagnosticKinds
    @enum T Error=0 Warning=1 Information=2 Hint=3
end

struct Diagnostic
    path::String
    line::Int
    severity::DiagnosticKinds.T
    msg::String
end

to_vscode_type(x::Diagnostic) = (msg=x.msg, path = x.path, line = x.line, severity = Int(x.severity))
function Base.show(io::IO, ::MIME"application/vnd.julia-vscode.diagnostics", diagnostics::AbstractVector{Diagnostic})
    return (
        source = "Cthulhu",
        items = to_vscode_type.(diagnostics),
    )
end

add_diagnostic!(::Nothing, node, position, severity) = nothing
function add_diagnostic!(diagnostics, node, position, severity)
    file_path = node.filename
    line, column = source_location(node, position)
    push!(diagnostics, Diagnostic(file_path, line, severity, "Unstable Type"))
end

function clear_diagnostics_vscode()
    if diagnostics_available_vscode()
        display(Main.VSCodeServer.InlineDisplay(false), TypedSyntax.Diagnostic[])
    end
end

function display_diagnostics_vscode(diagnostics)
    if diagnostics_available_vscode() && !isnothing(diagnostics)
        # InlineDisplay(false) means we don't print to REPL
        display(Main.VSCodeServer.InlineDisplay(false), diagnostics)
    end
end
display_diagnostics_vscode(io::IO) = display_diagnostics_vscode(get(io, :diagnostics, nothing))

const InlayHintKinds = (Type=1, Parameter=2, Nothing=nothing)

struct InlayHint
    line::Int
    column::Int
    label::String
    kind::Union{Nothing, Int}
end

to_vscode_type(x::InlayHint) = (position=(x.line, x.column), label=x.label, kind=x.kind)
function Base.show(io::IO, ::MIME"application/vnd.julia-vscode.inlayHints", inlay_hints_by_file::Dict{T, Vector{InlayHint}}) where T
    if inlay_hints_available_vscode()
        return Dict{T, Vector{NamedTuple{(:position, :label, :kind), Tuple{Tuple{Int, Int}, String, Union{Nothing, Int}}}}}(
            filepath => to_vscode_type.(inlay_hints) for (filepath, inlay_hints) in inlay_hints_by_file
        )
    end
    return nothing
end

add_hint!(::Nothing, message, node, position; kind=InlayHintKinds.Type) = nothing
function add_hint!(inlay_hints, message, node, position; kind=InlayHintKinds.Type)
    filepath = node.filename
    line, column = source_location(node, position)

    if filepath ∉ keys(inlay_hints)
        inlay_hints[filepath] = InlayHint[]
    end
    push!(inlay_hints[filepath], InlayHint(line-1, column, message, kind))
end

function clear_inlay_hints_vscode()
    if inlay_hints_available_vscode()
        display(Main.VSCodeServer.InlineDisplay(false),  Dict{String, Vector{TypedSyntax.InlayHint}}())
    end
end

function display_inlay_hints_vscode(inlay_hints)
    if inlay_hints_available_vscode() && !isnothing(inlay_hints)
        # InlineDisplay(false) means we don't print to REPL
        display(Main.VSCodeServer.InlineDisplay(false), inlay_hints)
    end
end
display_inlay_hints_vscode(io::IO) = display_inlay_hints_vscode(get(io, :inlay_hints, nothing))

function clear_all_vscode()
    clear_diagnostics_vscode()
    clear_inlay_hints_vscode()
end