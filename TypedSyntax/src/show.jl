## Extensions of JuliaSyntax to cover TypedSyntaxNode

function Base.show(io::IO, ::MIME"text/plain", node::TypedSyntaxNode; show_byte_offsets=false)
    println(io, "line:col│$(show_byte_offsets ? " byte_range  │" : "") tree                                   │ type")
    JuliaSyntax._show_syntax_node(io, Ref{Union{Nothing,String}}(nothing), node, "", show_byte_offsets)
end

function JuliaSyntax._show_syntax_node(io, current_filename, node::TypedSyntaxNode, indent, show_byte_offsets)
    line, col = source_location(node.source, node.position)
    posstr = "$(lpad(line, 4)):$(rpad(col,3))│"
    if show_byte_offsets
        posstr *= "$(lpad(first_byte(node),6)):$(rpad(last_byte(node),6))│"
    end
    val = node.val
    nodestr = !is_leaf(node) ? "[$(untokenize(head(node)))]" :
    isa(val, Symbol) ? string(val) : repr(val)
    treestr = string(indent, nodestr)
    if node.typ !== nothing
        treestr = string(rpad(treestr, 40), "│$(node.typ)")
    end
    println(io, posstr, treestr)
    if !is_leaf(node)
        new_indent = indent*"  "
        for n in children(node)
            JuliaSyntax._show_syntax_node(io, current_filename, n, new_indent, show_byte_offsets)
        end
    end
end

## Custom printing via `printstyled`

function Base.printstyled(io::IO, rootnode::MaybeTypedSyntaxNode;
                          type_annotations::Bool=true, iswarn::Bool=true, hide_type_stable::Bool=true,
                          with_linenumber::Bool=true,
                          idxend = last_byte(rootnode))
    rt = gettyp(rootnode)
    nd = with_linenumber ? ndigits_linenumbers(rootnode, idxend) : 0
    rootnode = get_function_def(rootnode)
    position = Int(first_byte(rootnode) - 1)
    with_linenumber && print_linenumber(io, rootnode, position + 1, nd)
    if is_function_def(rootnode)
        # We're printing a MethodInstance
        @assert length(children(rootnode)) == 2
        sig, body = children(rootnode)
        type_annotate, pre, pre2, post = type_annotation_mode(sig, rt; type_annotations, hide_type_stable)
        position = show_src_expr(io, sig, position, pre, pre2; type_annotations, iswarn, hide_type_stable, nd)
        type_annotate && show_annotation(io, rt, post, rootnode.source, position; iswarn)
        rootnode = body
    end
    position = show_src_expr(io, rootnode, position, "", ""; type_annotations, iswarn, hide_type_stable, nd)
    catchup(io, rootnode, position, nd, idxend+1)   # finish the node
    return nothing
end
Base.printstyled(rootnode::MaybeTypedSyntaxNode; kwargs...) = printstyled(stdout, rootnode; kwargs...)

ndigits_linenumbers(node::AbstractSyntaxNode, idxend = last_byte(node)) = ndigits(node.source.first_line + nlines(node.source, idxend) - 1)

function _print(io::IO, x, node, position)
    print(io, x)

    if !isempty(x)
        add_hint!(get(io, :inlay_hints, nothing), x, node, position+1)
    end
end

function show_src_expr(io::IO, node::MaybeTypedSyntaxNode, position::Int, pre::String, pre2::String; type_annotations::Bool=true, iswarn::Bool=false, hide_type_stable::Bool=false, nd::Int)
    _lastidx = last_byte(node)
    position = catchup(io, node, position, nd)
    if !is_leaf(node)
        position = catchup(io, first(children(node)), position, nd)
    end
    _print(io, pre, node.source, position)
    !is_leaf(node) && for (i, child) in enumerate(children(node))
        i == 2 && _print(io, pre2, node.source, position)
        cT = gettyp(child)
        ctype_annotate, cpre, cpre2, cpost = type_annotation_mode(child, cT; type_annotations, hide_type_stable)
        position = show_src_expr(io, child, position, cpre, cpre2; type_annotations, iswarn, hide_type_stable, nd)
        ctype_annotate && show_annotation(io, cT, cpost, node.source, position; iswarn)
    end
    return catchup(io, node, position, nd, _lastidx+1)
end

# should we print a type-annotation?
function is_show_annotation(@nospecialize(T); type_annotations::Bool, hide_type_stable::Bool)
    type_annotations || return false
    if isa(T, Core.Const)
        isa(T.val, Module) && return false
        T = Core.Typeof(T.val)
    end
    isa(T, Type) || return false
    hide_type_stable || return true
    return isa(T, Type) && is_type_unstable(T)
end

# Is the type equivalent to the source-text?
# We use `endswith` to handle module qualification
is_type_transparent(node, @nospecialize(T)) = endswith(replace(sprint(show, T), r"\s" => ""), replace(sourcetext(node), r"\s" => ""))

function is_callfunc(node::MaybeTypedSyntaxNode, @nospecialize(T))
    thisnode = node
    pnode = node.parent
    while pnode !== nothing && kind(pnode) ∈ KSet"quote ." && pnode.parent !== nothing
        thisnode = pnode
        pnode = pnode.parent
    end
    pnode === nothing && return false
    is_in_infix_context = kind(pnode) == K"op=" || kind(pnode) == K"call" && is_infix_op_call(pnode)
    is_caller_function = kind(pnode) == K"call" && thisnode === pnode.children[1]
    is_parametrized_type = kind(pnode) == K"curly" && thisnode === pnode.children[1]
    is_in_infix_context || is_caller_function || is_parametrized_type || return false
    if isa(T, Core.Const)
        T = T.val
    end
    if isa(T, Type) || isa(T, Function)
        T === Colon() && sourcetext(node) == ":" && return true
        return is_type_transparent(node, T)
    end
    return false
end

function type_annotation_mode(node, @nospecialize(T); type_annotations::Bool, hide_type_stable::Bool)
    kind(node) == K"return" && return false, "", "", ""
    is_callfunc(node, T) && return false, "", "", ""
    type_annotate = is_show_annotation(T; type_annotations, hide_type_stable)
    pre = pre2 = post = ""
    if type_annotate
        # Try stripping Core.Const and Type{T} wrappers to check if we need to avoid `String::Type{String}`
        # or `String::Core.Const(String)` annotations
        S = nothing
        if isa(T, Core.Const)
            val = T.val
            if isa(val, DataType)
                S = val
            end
        elseif isa(T, DataType) && T <: Type && isassigned(T.parameters, 1)
            S = T.parameters[1]
        end
        if S !== nothing && is_type_transparent(node, S)
            return false, pre, pre2, post
        end
        if kind(node) ∈ KSet":: where" || is_infix_op_call(node) || (is_prec_assignment(node) && kind(node) != K"=")
            pre, post = "(", ")"
        elseif is_prefix_op_call(node) # insert parens after prefix op and before type-annotating
            pre2, post = "(", ")"
        end
    end
    return type_annotate, pre, pre2, post
end

function show_annotation(io, @nospecialize(T), post, node, position; iswarn::Bool)
    diagnostics = get(io, :diagnostics, nothing)
    inlay_hints = get(io, :inlay_hints, nothing)

    print(io, post)
    if isa(T, Core.Const) && isa(T.val, Type)
        T = Type{T.val}
    end
    T_str = string(T)
    if iswarn && is_type_unstable(T)
        color = is_small_union_or_tunion(T) ? :yellow : :red
        printstyled(io, "::", T_str; color)
        add_diagnostic!(diagnostics, node, position+1, is_small_union_or_tunion(T) ? DiagnosticKinds.Information : DiagnosticKinds.Warning)
        add_hint!(inlay_hints, string(post, "::", T_str), node, position+1; kind=InlayHintKinds.Nothing)
    else
        printstyled(io, "::", T_str; color=:cyan)
        add_hint!(inlay_hints, string(post, "::", T_str), node, position+1; kind=InlayHintKinds.Type)
    end
end

print_linenumber(io::IO, node::MaybeTypedSyntaxNode, position::Int, nd::Int) =
    print_linenumber(io, source_line(node.source, position+1), nd)
print_linenumber(io::IO, ln::Int, nd::Int) = printstyled(io, lpad(ln, nd), " "; color=:light_black)

# Do any "overdue" printing, generating a line number if needed. Mostly, this catches whitespace.
# Printing occurs over indexes from `position:stop-1`.
function catchup(io::IO, node::MaybeTypedSyntaxNode, position::Int, nd::Int, stop = Int(first_byte(node)))
    if position + 1 < stop
        for (i, c) in pairs(node.source[position+1:stop-1])
            print(io, c)
            if c == '\n' && nd > 0
                print_linenumber(io, node, position + i + 1, nd)
            end
        end
        position = Int(stop - 1)
    end
    return position
end

nlines(source, idxend) = searchsortedfirst(source.line_starts, idxend)
nlines(source) = length(source.line_starts) - 1

is_type_unstable(@nospecialize(type)) = type isa Type && (!Base.isdispatchelem(type) || type == Core.Box)
function is_small_union_or_tunion(@nospecialize(T))
    Base.isvarargtype(T) && return false
    T === Union{} && return true
    if isa(T, Union)
        n, isc = countconcrete(T)
        return isc & (n <= 3)
    end
    if T <: Tuple  # is it Tuple{U}
        return all(is_small_union_or_tunion, (unwrap_unionall(T)::DataType).parameters)
    end
    return false
end

function countconcrete(@nospecialize(T))
    if Base.isdispatchelem(T)
        return 1, true
    elseif isa(T, Union)
        na, isca = countconcrete(T.a)
        nb, iscb = countconcrete(T.b)
        return na + nb, isca & iscb
    end
    return 0, false
end
