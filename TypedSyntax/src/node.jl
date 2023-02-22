
mutable struct TypedSyntaxData <: AbstractSyntaxData
    source::SourceFile
    typedsource::CodeInfo
    raw::GreenNode{SyntaxHead}
    position::Int
    val::Any
    typ::Any    # can either be a Type, `nothing`, or a `idxs::Vector{Int}` of *potential* call matches `src.code[idxs]`
end
TypedSyntaxData(sd::SyntaxData, src::CodeInfo, typ=nothing) = TypedSyntaxData(sd.source, src, sd.raw, sd.position, sd.val, typ)

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

function TypedSyntaxNode(rootnode::SyntaxNode, src::CodeInfo, Δline=0)
    mappings, symtyps = map_ssas_to_source(src, rootnode, Δline)
    node2ssa = IdDict{SyntaxNode,Int}(only(list) => i for (i, list) in pairs(mappings) if length(list) == 1)
    trootnode = TreeNode(nothing, nothing, TypedSyntaxData(rootnode.data, src, gettyp(node2ssa, rootnode, src)))
    addchildren!(trootnode, rootnode, src, node2ssa, symtyps)
    # Add argtyps to signature
    if is_functiondef(trootnode)
        sig, body = children(trootnode)
        @assert kind(sig) == K"call"
        i = 1
        for arg in Iterators.drop(children(sig), 1)
            @assert kind(arg) == K"Identifier"
            argname = arg.val
            while i <= length(src.slotnames)
                if src.slotnames[i] == argname
                    arg.typ = src.slottypes[i]
                    i += 1
                    break
                end
                i += 1
            end
        end
    end
    return trootnode
end

function addchildren!(tparent, parent, src::CodeInfo, node2ssa, symtyps)
    if haschildren(parent) && tparent.children === nothing
        tparent.children = TypedSyntaxNode[]
    end
    for child in children(parent)
        tnode = TreeNode(tparent, nothing, TypedSyntaxData(child.data, src, gettyp(node2ssa, child, src)))
        if tnode.typ === nothing && kind(child) == K"Identifier"
            tnode.typ = get(symtyps, child, nothing)
        end
        push!(tparent, tnode)
        addchildren!(tnode, child, src, node2ssa, symtyps)
    end
    if kind(tparent) == K"return" && haschildren(tparent)
        tparent.typ = only(children(tparent)).typ
    end
    return tparent
end

function gettyp(node2ssa, node, src)
    typ = get(node2ssa, node, nothing)
    if typ !== nothing
        typ = src.ssavaluetypes[typ]
    end
    return typ
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

function sparam_name(mi::MethodInstance, i::Int)
    sig = (mi.def::Method).sig::UnionAll
    while true
        i == 1 && break
        sig = sig.body::UnionAll
        i -= 1
    end
    return sig.var.name
end

function extract_call_name(stmt, mi)
    stmt.head ∈ (:call, :invoke) || return nothing
    f = stmt.args[1]
    if isa(f, GlobalRef) && f.mod === Core && f.name == :_apply_iterate   # handle vararg calls
        # Sanity check
        fiter = stmt.args[2]
        @assert isa(fiter, GlobalRef) && fiter.name == :iterate
        f = stmt.args[3]   # get the actual call
    end
    if isa(f, SlotNumber)
        return src.slotnames[f.id]
    end
    if isa(f, SSAValue)
        f = src.ssavaluetypes[f.id]
        if isa(fname, Core.Const)
            f = fname.val
        end
    end
    if isexpr(f, :static_parameter)
        return sparam_name(mi, f.args[1]::Int)
    end
    if isa(f, QuoteNode)
        f = f.val
    end
    isa(f, GlobalRef) && return f.name
    return nothing
end

function getsrc(@nospecialize(f), @nospecialize(t))
    srcrts = code_typed(f, t; debuginfo=:source, optimize=false)
    return only(srcrts)
end

function is_functiondef(node)
    kind(node) == K"function" && return true
    kind(node) == K"=" && kind(child(node, 1)) == K"call" && return true
    return false
end

# Start matching from the typed code rather than the source text
# The thinking here is that it may be easier to follow the flow of variables
# if we trace them through the SSAValues
function collect_symbol_nodes!(symlocs, node)
    kind(node) == K"->" && return symlocs     # skip inner functions (including `do` blocks below)
    is_functiondef(node) && return symlocs
    if !haschildren(node)
        name = node.val
        if isa(name, Symbol)
            locs = get(symlocs, name, nothing)
            if locs !== nothing
                push!(locs, node)
            end
        end
    else
        for c in (kind(node) == K"do" ? (child(node, 1),) : children(node))
            collect_symbol_nodes!(symlocs, c)
        end
    end
    return symlocs
end

function collect_symbol_nodes(rootnode, slotnames)
    kind(rootnode) ∈ KSet"function =" || error("expected function definition, got ", sourcetext(kind(rootnode)))
    symlocs = Dict{Symbol,Vector{typeof(rootnode)}}()
    for name in slotnames
        sname = string(name)
        (isempty(sname) || sname[1] == "#") && continue
        symlocs[name] = typeof(rootnode)[]
    end
    return collect_symbol_nodes!(symlocs, child(rootnode, 2))
end

function map_ssas_to_source(src, rootnode, Δline)
    symlocs = collect_symbol_nodes(rootnode, src.slotnames)      # all uses of slotnames
    mapping = [typeof(rootnode)[] for _ in eachindex(src.code)]  # attributions of stmts to sourcetext nodes
    symtyps = IdDict{typeof(rootnode),Any}()

    function append_targets_for_line!(mapped, i, targets)
        j = src.codelocs[i]
        linerange = src.linetable[j].line + Δline : (
                    j < length(src.linetable) ? src.linetable[j+1].line - 1  + Δline : typemax(Int))
        for t in targets
            source_line(t) ∈ linerange && push!(mapped, t)
        end
        return mapped
    end
    function append_targets_for_arg!(mapped, i, arg)
        if isa(arg, SlotNumber)
            targets = get(symlocs, src.slotnames[arg.id], nothing)
            if targets !== nothing
                append_targets_for_line!(mapped, i, targets)
            end
        elseif isa(arg, SSAValue)
            append!(mapped, mapping[arg.id])
        end
        return mapped
    end

    for (i, mapped, stmt) in zip(eachindex(mapping), mapping, src.code)
        if isa(stmt, SlotNumber) || isa(stmt, SSAValue)
            append_targets_for_arg!(mapped, i, stmt)
        elseif isa(stmt, Expr)
            if stmt.head == :(=)
                stmt = stmt.args[2]
                if isa(stmt, SlotNumber) || isa(stmt, SSAValue)
                    append_targets_for_arg!(mapped, i, stmt)
                    continue
                end
                @assert isa(stmt, Expr)
            end
            argmapping = typeof(rootnode)[]
            stmtmapping = Set{typeof(rootnode)}()
            for arg in stmt.args
                append_targets_for_arg!(argmapping, i, arg)
                if !isempty(argmapping)
                    if isempty(stmtmapping)
                        foreach(argmapping) do t
                            push!(stmtmapping, t.parent)
                        end
                    else
                        intersect!(stmtmapping, map(t->t.parent, argmapping))
                    end
                end
                empty!(argmapping)
            end
            fname = extract_call_name(stmt, src.parent)
            if fname !== nothing
                for target in stmtmapping
                    srcname = kind(target) == K"ref" ? "ref" : sourcetext(target.children[1 + is_infix_op_call(target)])
                    if string(fname) == srcname || (fname ∈ (:getindex, :setindex!) && srcname == "ref")
                        push!(mapped, target)
                    end
                end
                !isempty(stmtmapping) && isempty(mapped) && @warn "no name match for $stmt"
            else
                stmt.head != :new && @warn "didn't map as call: $stmt"
                # Not sure what to do here
                append!(mapped, stmtmapping)
            end
            sort!(mapped; by=t->t.position)   # since they went into a set
            if length(mapped) == 1 && isa(stmt, Expr)
                node = only(mapped)
                # set up symtyps for this call
                if stmt.head == :(=)
                    arg = stmt.args[1]
                    if isa(arg, SlotNumber)
                        sym = src.slotnames[arg.id]
                        for t in symlocs[sym]
                            if t.parent == node
                                symtyps[t] = src.ssavaluetypes[i]
                                break
                            end
                        end
                    end
                    stmt = stmt.args[2]
                end
                if isa(stmt, Expr)
                    for arg in stmt.args
                        j = 0
                        while isa(arg, SSAValue)
                            j = arg.id
                            arg = src.ssavaluetypes[j]
                        end
                        if isa(arg, SlotNumber)
                            sym = src.slotnames[arg.id]
                            for t in symlocs[sym]
                                if t.parent == node
                                    symtyps[t] = if j > 0
                                        src.ssavaluetypes[j]
                                    else
                                        j = findfirst(==(sym), src.slotnames)
                                        src.slottypes[j]
                                    end
                                    break
                                end
                            end
                        end
                    end
                end
            end
        end
    end
    return mapping, symtyps
end
