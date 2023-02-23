function Base.show(io::IO, ::MIME"text/plain", node::TypedSyntaxNode)
    println(io, "line:col│ byte_range  │ tree                                   │ type or call idxs")
    JuliaSyntax._show_syntax_node(io, Ref{Union{Nothing,String}}(nothing), node, "")
end

function JuliaSyntax._show_syntax_node(io, current_filename, node::TypedSyntaxNode, indent)
    fname = node.source.filename
    line, col = source_location(node.source, node.position)
    posstr = "$(lpad(line, 4)):$(rpad(col,3))│$(lpad(first_byte(node),6)):$(rpad(last_byte(node),6))│"
    val = node.val
    nodestr = haschildren(node) ? "[$(untokenize(head(node)))]" :
    isa(val, Symbol) ? string(val) : repr(val)
    treestr = string(indent, nodestr)
    if node.typ !== nothing
        treestr = string(rpad(treestr, 40), "│$(node.typ)")
    end
    println(io, posstr, treestr)
    if haschildren(node)
        new_indent = indent*"  "
        for n in children(node)
            JuliaSyntax._show_syntax_node(io, current_filename, n, new_indent)
        end
    end
end


function Base.printstyled(io::IO, rootnode::TypedSyntaxNode; iswarn::Bool=true, hide_type_stable::Bool=true, kwargs...)
    rt = rootnode.typ
    rootnode = get_function_def(rootnode)
    lastidx = 0
    if is_function_def(rootnode)
        # We're printing a MethodInstance
        @assert length(children(rootnode)) == 2
        sig, body = children(rootnode)
        lastidx = show_src_expr(io, sig, lastidx; iswarn, hide_type_stable)
        maybe_show_annotation(io, rt; iswarn, hide_type_stable)
        rootnode = body
    end
    lastidx = show_src_expr(io, rootnode, lastidx; iswarn, hide_type_stable)
    println(io, rootnode.source[lastidx+1:end])
end

function show_src_expr(io::IO, node::TypedSyntaxNode, lastidx::Int; iswarn::Bool=false, hide_type_stable::Bool=false)
    lastidx = catchup(io, node, lastidx)
    _lastidx = last_byte(node)
    if kind(node) == K"Identifier" || (kind(node) == K"::" && is_prefix_op_call(node))
        print(io, node.source[lastidx+1:_lastidx])
        maybe_show_annotation(io, node.typ; iswarn, hide_type_stable)
        return _lastidx
    end
    # We only handle "call" nodes. For anything else, just print the node (recursing into children)
    if kind(node) != K"call"
        for child in children(node)
            lastidx = show_src_expr(io, child, lastidx; iswarn, hide_type_stable)
        end
        print(io, node.source[lastidx+1:_lastidx])
        return _lastidx
    end
    pre = prepost = post = ""
    if is_infix_op_call(node)   # wrap infix calls in parens before type-annotating
        pre, post = "(", ")"
        lastidx = catchup(io, first(children(node)), lastidx)
    elseif is_prefix_op_call(node) # insert parens after prefix op and before type-annotating
        prepost, post = "(", ")"
    end
    T = node.typ
    # should we print a type-annotation?
    type_annotate = isa(T, Vector{Int}) || (isa(T, Type) && (!hide_type_stable || is_type_unstable(T)))
    type_annotate && print(io, pre)
    for (childid, child) in enumerate(children(node))
        childid == 2 && type_annotate && print(io, prepost)
        lastidx = show_src_expr(io, child, lastidx; iswarn, hide_type_stable)
    end
    print(io, node.source[lastidx+1:_lastidx])
    if type_annotate
        show_annotation(io, T, post; iswarn)
    end
    return _lastidx
end

function maybe_show_annotation(io, @nospecialize(T); iswarn, hide_type_stable)
    if isa(T, Core.Const)
        T = typeof(T.val)
    end
    if isa(T, Type) && (!hide_type_stable || is_type_unstable(T))   # should we print a type-annotation?
        show_annotation(io, T; iswarn)
    end
end

function show_annotation(io, @nospecialize(T), post=""; iswarn::Bool)
    print(io, post)
    if isa(T, Vector{Int})
        isempty(T) && return
        if iswarn
            printstyled(io, "::NF"; color=:yellow)
        else
            print(io, "::NF")
        end
        return
    end
    if iswarn
        color = !is_type_unstable(T) ? :cyan :
                 is_small_union_or_tunion(T) ? :yellow : :red
        printstyled(io, "::", T; color)
    else
        print(io, "::", T)
    end
end

function catchup(io::IO, node::TypedSyntaxNode, lastidx::Int)
    # Do any "overdue" printing now. Mostly, this catches whitespace
    firstidx = first_byte(node)
    if lastidx + 1 < firstidx
        print(io, node.source[lastidx+1:firstidx-1])
        lastidx = firstidx-1
    end
    return lastidx
end

is_type_unstable(@nospecialize(type)) = type isa Type && (!Base.isdispatchelem(type) || type == Core.Box)
function is_small_union_or_tunion(@nospecialize(T))
    Base.isvarargtype(T) && return false
    if T <: Tuple   # is it Tuple{U}
        return all(is_small_union_or_tunion, Base.unwrap_unionall(T).parameters)
    end
    isa(T, Union) || return false
    n, isc = countconcrete(T)
    return isc & (n <= 3)
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