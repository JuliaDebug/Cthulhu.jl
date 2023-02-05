using Core: CodeInfo, MethodInstance
using JuliaSyntax: SyntaxNode, head, children, untokenize, first_byte, last_byte, flags, EMPTY_FLAGS, INFIX_FLAG, @K_str
using Cthulhu: is_type_unstable

function show_src_signature(io::IO, lineno::Int32, @nospecialize(rt), mi::MethodInstance, signode::SyntaxNode;
                            iswarn::Bool=false, hide_type_stable::Bool=false)
    hdtok = untokenize(head(signode))
    hdtok == "call" || error("expected call signature, got ", hd)
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

function match_call(callname, src, taken)
    for i in eachindex(taken)
        taken[i] && continue
        stmt = src.code[i]
        isa(stmt, Expr) || continue
        stmt.head ∈ (:call, :invoke) || continue
        f = stmt.args[1]
        @assert isa(f, GlobalRef)
        string(f.name) == callname && return i
    end
    return nothing
end

function show_src_expr!(io::IO, taken, src::CodeInfo, lineno::Int32, node::SyntaxNode, lastidx::Int;
                        iswarn::Bool=false, hide_type_stable::Bool=false)
    hd = head(node)
    srctxt = node.source.code
    _lastidx = last_byte(node)
    if flags(hd) === EMPTY_FLAGS && hd.kind != K"call"
        print(io, srctxt[lastidx+1:_lastidx])
        return _lastidx
    end
    pre = post = ""
    callname = node.val[1]
    if !iszero(flags(hd) & INFIX_FLAG)
        pre, post = "(", ")"
        callname = node.val[2]
    end
    idx = match_call(string(callname), src, taken)
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

function show_annotated(io::IO, src::CodeInfo, lineno::Int32, @nospecialize(rt), mi::MethodInstance, rootnode::SyntaxNode;
                        iswarn::Bool=false, hide_type_stable::Bool=false)
    # Function signature
    hd = untokenize(head(rootnode))
    hd == "=" || hd == "function" || error("expected function definition, got head ", hd)
    length(rootnode.val) == 2 || error("expected sig, body args, got ", length(rootnode.val), " children")
    signode = rootnode.val[1]
    lastidx = show_src_signature(io, lineno, rt, mi, signode; iswarn, hide_type_stable)
    bodynode = rootnode.val[2]
    taken = fill(false, length(src.code))
    for node in children(bodynode)
        lastidx = show_src_expr!(io, taken, src, lineno, node, lastidx; iswarn, hide_type_stable)
    end
    srctxt = rootnode.source.code
    print(io, srctxt[lastidx+1:end])
end
