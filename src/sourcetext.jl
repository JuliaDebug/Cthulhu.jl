using Core: CodeInfo, MethodInstance
using JuliaSyntax: JuliaSyntax, SyntaxNode, head, kind, children, untokenize, first_byte, last_byte,
                   flags, EMPTY_FLAGS, INFIX_FLAG, @K_str, source_location
using Cthulhu: is_type_unstable

function show_src_signature(io::IO, lineno::Int, @nospecialize(rt), mi::MethodInstance, signode::SyntaxNode;
                            iswarn::Bool=false, hide_type_stable::Bool=false)
    hdtok = untokenize(head(signode))
    if hdtok == "where"
       signode = signode.val[1]
       hdtok = untokenize(head(signode))
    end
    hdtok == "call" || error("expected call signature, got ", hdtok)
    args = signode.val[2:end]
    srctxt = signode.source.code
    sigT = Base.unwrap_unionall(mi.specTypes)
    lastidx = first_byte(first(args))-1
    print(io, srctxt[1:lastidx])
    for (i, arg) in enumerate(args)
        T = sigT.parameters[i+1]
        hd = head(arg)
        if flags(hd) === EMPTY_FLAGS
            _lastidx = last_byte(arg)
            print(io, srctxt[lastidx+1:_lastidx])
            lastidx = _lastidx
            if !hide_type_stable || is_type_unstable(T)
                print(io, "::")
                iswarn && is_type_unstable(T) ? printstyled(io, T; color=:red) : print(io, T)
            end
        elseif untokenize(hd) == "::-i"
            if !is_type_unstable(T) && hide_type_stable
                # Should still print the declared type
                _lastidx = last_byte(arg)
                print(io, srctxt[lastidx+1:_lastidx])
                lastidx = _lastidx
            else
                # Print the specialized type
                varname, vartype = arg.val
                _lastidx = last_byte(varname)
                print(io, srctxt[lastidx+1:_lastidx])
                print(io, "::")
                iswarn && is_type_unstable(T) ? printstyled(io, T; color=:red) : print(io, T)
                lastidx = last_byte(arg)
            end
        else
            error("unhandled head ", hd)
        end
    end
    # Print the closing ')' and the return type
    l = length(srctxt)
    lastidx += 1
    while lastidx <= l
        c = srctxt[lastidx]
        print(io, c)
        c == ')' && break
        lastidx = nextind(srctxt, lastidx)
    end
    print(io, "::")
    iswarn && is_type_unstable(rt) ? printstyled(io, rt; color=:red) : print(io, rt)
    return lastidx
end

function match_call(callname, src, lineno, taken)
    clidx = findfirst(src.linetable) do lin
        lin.line == lineno
    end
    clidx === nothing && return nothing # `do`-block internal functions & similar; @show src.linetable lineno callname
    @assert clidx == length(src.linetable) || src.linetable[clidx+1].line > lineno
    for i in eachindex(taken)
        src.codelocs[i] == clidx || continue
        taken[i] && continue
        stmt = src.code[i]
        isa(stmt, Expr) || continue
        if stmt.head == :(=)
            stmt = stmt.args[2]
            isa(stmt, Expr) || continue
        end
        stmt.head ∈ (:call, :invoke) || continue
        f = stmt.args[1]
        (isa(f, Core.SlotNumber) || isa(f, Core.SSAValue)) && (@warn("unhandled slot or SSAValue in $stmt"); continue)
        isa(f, GlobalRef) || error("expected GlobalRef, got ", f)
        string(f.name) == callname && return i
    end
    return nothing
end

function show_src_expr!(io::IO, taken, src::CodeInfo, lineno::Int, node::SyntaxNode, lastidx::Int;
                        iswarn::Bool=false, hide_type_stable::Bool=false)
    hd = head(node)
    srctxt = node.source.code
    _lastidx = last_byte(node)
    if kind(hd) != K"call"
        for child in children(node)
            lastidx = show_src_expr!(io, taken, src, lineno, child, lastidx; iswarn, hide_type_stable)
        end
        print(io, srctxt[lastidx+1:_lastidx])
        return _lastidx
    end
    pre = post = ""
    callname = node.val[1]
    if !iszero(flags(hd) & INFIX_FLAG)
        pre, post = "(", ")"
        callname = node.val[2]
    end
    line, _ = source_location(node.source, node.position)
    idx = match_call(string(callname), src, line + lineno - 1, taken)
    if idx === nothing
        @warn "unmatched call $node"
        print(io, srctxt[lastidx+1:_lastidx])
        return _lastidx
    end
    taken[idx] = true
    T = src.ssavaluetypes[idx]
    type_annotate = !hide_type_stable || is_type_unstable(T)
    type_annotate && print(io, pre)
    for child in children(node)
        lastidx = show_src_expr!(io, taken, src, lineno, child, lastidx; iswarn, hide_type_stable)
    end
    print(io, srctxt[lastidx+1:_lastidx])
    if type_annotate
        print(io, post, "::")
        iswarn && is_type_unstable(T) ? printstyled(io, T; color=:red) : print(io, T)
    end
    return _lastidx
end

function show_annotated(io::IO, src::CodeInfo, lineno::Int, @nospecialize(rt), mi::MethodInstance, rootnode::SyntaxNode;
                        iswarn::Bool=false, hide_type_stable::Bool=false)
    # Function signature
    hd = untokenize(head(rootnode))
    while hd != "=" && hd != "function"
        hd == "macrocall" || error("expected function definition, got head ", hd)
        rootnode = rootnode.val[end]
        hd = untokenize(head(rootnode))
    end
    length(rootnode.val) == 2 || error("expected sig, body args, got ", length(rootnode.val), " children")
    signode = rootnode.val[1]
    # FIXME? use the line number when the method was created. This is needed
    # for call-matching but might mess things up if we someday print line numbers in the left margin
    lineno = Int(mi.def.line)
    lastidx = show_src_signature(io, lineno, rt, mi, signode; iswarn, hide_type_stable)
    bodynode = rootnode.val[2]
    taken = fill(false, length(src.code))
    for node in children(bodynode)
        lastidx = show_src_expr!(io, taken, src, lineno, node, lastidx; iswarn, hide_type_stable)
    end
    srctxt = rootnode.source.code
    print(io, srctxt[lastidx+1:end])
end
