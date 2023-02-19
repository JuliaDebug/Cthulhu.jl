
mutable struct TypedSyntaxData <: AbstractSyntaxData
    source::SourceFile
    typedsource::CodeInfo
    raw::GreenNode{SyntaxHead}
    position::Int
    val::Any
    typ::Any    # can either be a Type, `nothing`, or a `idxs::Vector{Int}` of *potential* call matches `src.code[idxs]`
end

const TypedSyntaxNode = TreeNode{TypedSyntaxData}

struct NotFound end
# struct Unmatched end

function TypedSyntaxNode(@nospecialize(f), @nospecialize(t); kwargs...)
    m = which(f, t)
    sourcetext, lineno = definition(String, m)
    rootnode = JuliaSyntax.parse(SyntaxNode, sourcetext; filename=string(m.file), first_line=lineno, kwargs...)
    src, rt = getsrc(f, t)
    node = TypedSyntaxNode(rootnode, src, lineno - m.line)
    node.val = rt
    return node
end

function TypedSyntaxNode(node::SyntaxNode, src::CodeInfo, Δline=0)
    taken = [TypedSyntaxNode[] for _ = 1:length(src.code)]
    tnode = typednode_pass1(node, src, nothing, taken, Δline, true)   # pass1 finds all possible matches
    tnode = typednode_pass2!(tnode, src, taken)
    tnode = typednode_pass3!(tnode)
    return tnode
end

# During pass1, we list all possible matches for each call.
# Then during pass2, we use the fact that inference preserves order to say that if there are the
# same number of call sites in the sourcetext as we found in the typed code,
# we can line them up one-to-one.
# A case where that *won't* happen is a line like `ntuple(i -> a[i], b[j])`, where the anonymous
# function will not be in `src` and so there will be two `getindex` calls in the sourcetext but only one in `src`.
function typednode_pass1(node::SyntaxNode, src::CodeInfo, parent, taken, Δline, mayassign::Bool)
    hd = head(node)
    sd = node.data
    if kind(hd) == K"Identifier"
        # typed value
        slotidx = findfirst(==(node.val::Symbol), src.slotnames)
        T = slotidx === nothing ? NotFound : src.slottypes[slotidx]
        tsd = TypedSyntaxData(sd.source, src, sd.raw, sd.position, sd.val, T)
        return TreeNode(parent, nothing, tsd)
    end
    tsd = if (kind(hd) == K"call" || is_getindex(node)) && haschildren(node) && mayassign
        line = source_line(node.source, node.position)
        calltok = kind(hd) == K"ref" ? "getindex" : JuliaSyntax.sourcetext(node.children[1 + is_infix_op_call(hd)])
        codeidxs = match_call(calltok, src, line - Δline, src.parent)  # FIXME: match arg types too
        TypedSyntaxData(sd.source, src, sd.raw, sd.position, sd.val, codeidxs)
    else
        TypedSyntaxData(sd.source, src, sd.raw, sd.position, sd.val, nothing)
    end
    newparent = TreeNode(parent, #= replaceme after constructing children =# nothing, tsd)
    if haschildren(node)
        # Avoid assigning in code that is hidden in anonymous functions
        if kind(hd) == K"->"
            mayassign = false
        end
        mayassign2 = mayassign
        if kind(hd) == K"do"  # in a do block we can
            mayassign2 = false
        end
        newchildren = TypedSyntaxNode[]
        for child in children(node)
            push!(newchildren, typednode_pass1(child, src, newparent, taken, Δline, mayassign))
            mayassign = mayassign2
        end
        newparent.children = newchildren
    end
    if isa(tsd.typ, Vector{Int})
        foreach(tsd.typ) do i
            push!(taken[i], newparent)
        end
    end
    return newparent
end

function typednode_pass2!(tnode, src, taken)
    # Resolve all the calls that can be unambiguously resolved
    n = length(src.code)
    for i = 1:n
        t = taken[i]
        len = length(t)
        if len == 1
            only(t).typ = src.ssavaluetypes[i]
            empty!(t)
        elseif len > 1
            # Multiple calls map to this one. If the number mapping is equal to the number of duplicates,
            # we can unambiguously assign them by order
            mapsame, lineidx = [i], src.codelocs[i]
            j = i + 1
            while j <= n && src.codelocs[j] == lineidx
                if taken[j] == t
                    push!(mapsame, j)
                end
                j += 1
            end
            if length(mapsame) == len
                # Successive `ref`s `a[i][j]` have to be reverse-ordered
                tidx = 1
                while tidx <= len
                    titem = t[tidx]
                    if kind(titem) == K"ref"
                        tidxlastref = tidx
                        while tidxlastref < len && (nextnode = t[tidxlastref + 1]; kind(nextnode) == K"ref")
                            titem.parent == nextnode || break
                            tidxlastref += 1
                            titem = nextnode
                        end
                        if tidxlastref != tidx
                            reverse!(view(t, tidx:tidxlastref))
                        end
                        tidx = tidxlastref + 1
                    else
                        tidx += 1
                    end
                end
                for (tidx, j) in enumerate(mapsame)
                    t[tidx].typ = src.ssavaluetypes[j]
                end
                for j in mapsame
                    empty!(taken[j])
                end
            end
        end
    end
    return tnode  # while this is the only direct use of `tnode`, we accessed it via `taken` so this is clearer
end

# Propagate typ upward in tree
function typednode_pass3!(tnode)
    for child in children(tnode)
        typednode_pass3!(child)
    end
    if kind(tnode) == K"return"
        tnode.typ = only(children(tnode)).typ
    end
    return tnode
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

function match_call(callname, src, lineno, mi)
    clidx = find_codeloc(src, lineno)
    codeidxs = Int[]
    i, n = searchsortedfirst(src.codelocs, clidx) - 1, lastindex(src.codelocs)
    while i < n
        i += 1
        src.codelocs[i] == clidx || break
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
            if string(fname) == callname
                push!(codeidxs, i)
                continue
            end
        end
        if isa(f, Core.SSAValue)
            fname = src.ssavaluetypes[f.id]
            if isa(fname, Core.Const)
                fname = fname.val
            end
            fname = string(fname)
            if (endswith(fname, callname) || endswith(callname, fname))
                push!(codeidxs, i)
                continue
            end
        end
        if isa(f, Core.SlotNumber) || isa(f, Core.SSAValue)
            @warn "unhandled slot or SSAValue in $stmt"
            continue
        end
        if isexpr(f, :static_parameter)
            varname = sparam_name(mi, f.args[1]::Int)
            callname == varname && push!(codeidxs, i)
            continue
        end
        isa(f, GlobalRef) || error("expected GlobalRef, got ", f)
        string(f.name) == callname && push!(codeidxs, i)
    end
    return codeidxs
end

function sparam_name(mi::MethodInstance, i::Int)
    sig = (mi.def::Method).sig::UnionAll
    while true
        i == 1 && break
        sig = sig.body::UnionAll
        i -= 1
    end
    return sig.var.name
end

function getsrc(@nospecialize(f), @nospecialize(t))
    srcrts = code_typed(f, t; debuginfo=:source, optimize=false)
    return only(srcrts)
end

function is_getindex(node)
    kind(node) == K"ref" || return false
    # is this `getindex` or `setindex!`?
    pnode = node.parent
    kind(pnode) == K"=" || return true
    return child(pnode, 1) !== node
end
