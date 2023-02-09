using Core: CodeInfo, MethodInstance
using JuliaSyntax: JuliaSyntax, SyntaxNode, head, kind, children, haschildren, untokenize, first_byte, last_byte,
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

function stringify(calltok::SyntaxNode)
    if !haschildren(calltok)
        val = calltok.val
        return isa(val, Symbol) ? string(val) : repr(val)
    end
    h = untokenize(head(calltok))
    h == "." || error("unhandled calltok ", calltok)
    length(calltok.val) == 2 || error("unhandled calltok ", calltok)
    kind(calltok[1]) == K"Identifier" || error("unhandled calltok ", calltok)
    kind(calltok[2]) == K"quote" || error("unhandled calltok ", calltok)
    return stringify(calltok.val[1]) * "." * stringify(calltok.val[2].val[1])
end

function find_codeloc(src, lineno)
    clidx = searchsortedfirst(src.linetable, lineno; lt=(linenode, line) -> linenode.line < line)
    clidx = min(clidx, length(src.linetable))
    if src.linetable[clidx].line > lineno
        clidx -= 1    # handle multiline statements
    end
    return clidx
end
function find_coderange(src, lineno)
    clidx = find_codeloc(src, lineno)
    ibegin = searchsortedfirst(src.codelocs, clidx)
    ibegin += src.codelocs[ibegin] > lineno
    iend = searchsortedlast(src.codelocs, clidx)
    return ibegin:iend
end

function match_call(callname, src, lineno, taken)
    @assert axes(taken) == axes(src.code)
    clidx = find_codeloc(src, lineno)
    i = searchsortedfirst(src.codelocs, clidx) - 1
    while i < lastindex(taken)
        i += 1
        src.codelocs[i] == clidx || break
        taken[i] && continue
        stmt = src.code[i]
        isa(stmt, Expr) || continue
        if stmt.head == :(=)
            stmt = stmt.args[2]
            isa(stmt, Expr) || continue
        end
        stmt.head ∈ (:call, :invoke) || continue
        f = stmt.args[1]
        if isa(f, GlobalRef) && f.mod === Core && f.name == :_apply_iterate   # handle vararg calls
            # Sanity check
            fiter = stmt.args[2]
            @assert isa(fiter, GlobalRef) && fiter.name == :iterate
            f = stmt.args[3]   # get the actual call
        end
        if isa(f, Core.SlotNumber)
            fname = src.slotnames[f.id]
            string(fname) == callname && return i
        end
        if isa(f, Core.SSAValue)
            fname = src.ssavaluetypes[f.id]
            if isa(fname, Core.Const)
                fname = fname.val
            end
            fname = string(fname)
            (endswith(fname, callname) || endswith(callname, fname)) && return i
        end
        if isa(f, Core.SlotNumber) || isa(f, Core.SSAValue)
            @warn "unhandled slot or SSAValue in $stmt"
            continue
        end
        isa(f, GlobalRef) || error("expected GlobalRef, got ", f)
        string(f.name) == callname && return i
    end
    return nothing
end

function show_src_expr!(io::IO, taken, src::CodeInfo, lineno::Int, node::SyntaxNode, lastidx::Int;
                        iswarn::Bool=false, hide_type_stable::Bool=false)
    hd = head(node)
    srctxt = node.source.code
    firstidx, _lastidx = first_byte(node), last_byte(node)
    if lastidx + 1 < firstidx    # do any "overdue" printing now. Mostly, this catches whitespace
        print(io, srctxt[lastidx+1:firstidx-1])
        lastidx = firstidx-1
    end
    # We only handle "call" nodes. For anything else, just print the node (recursing into children)
    if kind(hd) != K"call"
        for child in children(node)
            lastidx = show_src_expr!(io, taken, src, lineno, child, lastidx; iswarn, hide_type_stable)
        end
        print(io, srctxt[lastidx+1:_lastidx])
        return _lastidx
    end
    pre = post = ""
    calltok = node.val[1]
    if !iszero(flags(hd) & INFIX_FLAG)   # wrap infix calls in parens before type-annotating
        pre, post = "(", ")"
        calltok = node.val[2]
    end
    # Connect the call in the raw source text to an inferred call in `src`
    line, _ = source_location(node.source, node.position)
    idx = match_call(stringify(calltok), src, line + lineno - 1, taken)
    if idx === nothing
        # @warn "unmatched call $node on line $(line+lineno-1)"
        # rng = find_coderange(src, line + lineno - 1)
        # display(src.code[rng][(!).(taken[rng])])
        print(io, srctxt[lastidx+1:_lastidx])
        return _lastidx
    end
    taken[idx] = true  # mark it as taken (in case there are multiple calls to the same function on the same line)
    T = src.ssavaluetypes[idx]
    type_annotate = !hide_type_stable || is_type_unstable(T)   # should we print a type-annotation?
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
    # FIXME: `lineno` will be useful for handling edited (`Revise`d) files
    # Function signature
    k = kind(rootnode)
    while k != K"=" && k != K"function"
        k == K"macrocall" || error("expected function definition, got kind ", k)
        rootnode = rootnode.val[end]
        k = kind(rootnode)
    end
    length(rootnode.val) == 2 || error("expected sig, body args, got ", length(rootnode.val), " children")
    signode = rootnode.val[1]
    mlineno = Int(mi.def.line)   # line number at the time of method definition (used for matching)
    lastidx = show_src_signature(io, mlineno, rt, mi, signode; iswarn, hide_type_stable)
    bodynode = rootnode.val[2]
    taken = fill(false, length(src.code))
    for node in children(bodynode)
        lastidx = show_src_expr!(io, taken, src, mlineno, node, lastidx; iswarn, hide_type_stable)
    end
    srctxt = rootnode.source.code
    print(io, srctxt[lastidx+1:end])
end
