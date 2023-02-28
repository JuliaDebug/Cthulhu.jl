
mutable struct TypedSyntaxData <: AbstractSyntaxData
    source::SourceFile
    typedsource::CodeInfo
    raw::GreenNode{SyntaxHead}
    position::Int
    val::Any
    typ::Any    # can either be a Type or `nothing`
end
TypedSyntaxData(sd::SyntaxData, src::CodeInfo, typ=nothing) = TypedSyntaxData(sd.source, src, sd.raw, sd.position, sd.val, typ)

const TypedSyntaxNode = TreeNode{TypedSyntaxData}
const MaybeTypedSyntaxNode = Union{SyntaxNode,TypedSyntaxNode}

struct NotFound end
# struct Unmatched end

function TypedSyntaxNode(@nospecialize(f), @nospecialize(t); kwargs...)
    m = which(f, t)
    sourcetext, lineno = definition(String, m)
    rootnode = JuliaSyntax.parse(SyntaxNode, sourcetext; filename=string(m.file), first_line=lineno, kwargs...)
    src, rt = getsrc(f, t)
    node = TypedSyntaxNode(rootnode, src, lineno - m.line)
    node.typ = rt
    return node
end

TypedSyntaxNode(rootnode::SyntaxNode, src::CodeInfo, Δline::Integer=0) =
    TypedSyntaxNode(rootnode, src, map_ssas_to_source(src, rootnode, Δline)...)

function TypedSyntaxNode(rootnode::SyntaxNode, src::CodeInfo, mappings, symtyps)
    # There may be ambiguous assignments back to the source; preserve just the unambiguous ones
    node2ssa = IdDict{SyntaxNode,Int}(only(list) => i for (i, list) in pairs(mappings) if length(list) == 1)
    # Copy `rootnode`, adding type annotations
    trootnode = TreeNode(nothing, nothing, TypedSyntaxData(rootnode.data, src, gettyp(node2ssa, rootnode, src)))
    addchildren!(trootnode, rootnode, src, node2ssa, symtyps, mappings)
    # Add argtyps to signature
    fnode = get_function_def(trootnode)
    if is_function_def(fnode)
        sig, body = children(fnode)
        if kind(sig) == K"where"
            sig = child(sig, 1)
        end
        @assert kind(sig) == K"call"
        i = 1
        for arg in Iterators.drop(children(sig), 1)
            kind(arg) == K"parameters" && break   # kw args
            if kind(arg) == K"..."
                arg = only(children(arg))
            end
            if kind(arg) == K"::"
                nchildren = length(children(arg))
                if nchildren == 1
                    # unnamed argument
                    found = false
                    while i <= length(src.slotnames)
                        if src.slotnames[i] == Symbol("#unused#")
                            arg.typ = src.slottypes[i]
                            i += 1
                            found = true
                            break
                        end
                        i += 1
                    end
                    @assert found
                    continue
                elseif nchildren == 2
                    arg = child(arg, 1)  # extract the name
                else
                    error("unexpected number of children: ", children(arg))
                end
            end
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
    # foreach(mappings) do mapped
    #     if any(n -> !isa(n, TypedSyntaxNode), mapped)
    #         display(src.parent)
    #         display(rootnode)
    #         error("hoped that all would be typed, got ", mapped)
    #     end
    # end
    return trootnode
end

function addchildren!(tparent, parent, src::CodeInfo, node2ssa, symtyps, mappings)
    if haschildren(parent) && tparent.children === nothing
        tparent.children = TypedSyntaxNode[]
    end
    for child in children(parent)
        tnode = TreeNode(tparent, nothing, TypedSyntaxData(child.data, src, gettyp(node2ssa, child, src)))
        if tnode.typ === nothing && kind(child) == K"Identifier"
            tnode.typ = get(symtyps, child, nothing)
        end
        push!(tparent, tnode)
        addchildren!(tnode, child, src, node2ssa, symtyps, mappings)
    end
    # In `return f(args..)`, copy any types assigned to `f(args...)` up to the `[return]` node
    if kind(tparent) == K"return" && haschildren(tparent)
        tparent.typ = only(children(tparent)).typ
    end
    # Replace the entry in `mappings` to be the typed node
    i = get(node2ssa, parent, nothing)
    if i !== nothing
        @assert length(mappings[i]) == 1
        mappings[i][1] = tparent
    end
    return tparent
end

function gettyp(node2ssa, node, src)
    i = get(node2ssa, node, nothing)
    i === nothing && return nothing
    stmt = src.code[i]
    if isa(stmt, Core.ReturnNode)
        arg = stmt.val
        isa(arg, SSAValue) && return src.ssavaluetypes[arg.id]
        is_slot(arg) && return src.slottypes[arg.id]
    end
    return src.ssavaluetypes[i]
end

Base.copy(tsd::TypedSyntaxData) = TypedSyntaxData(tsd.source, tsd.typedsource, tsd.raw, tsd.position, tsd.val, tsd.typ)

gettyp(node::AbstractSyntaxNode) = gettyp(node.data)
gettyp(::JuliaSyntax.SyntaxData) = nothing
gettyp(data::TypedSyntaxData) = data.typ

# function find_codeloc(src, lineno)
#     clidx = searchsortedfirst(src.linetable, lineno; lt=(linenode, line) -> linenode.line < line)
#     clidx = min(clidx, length(src.linetable))
#     if src.linetable[clidx].line > lineno
#         clidx -= 1    # handle multiline statements
#     end
#     return clidx
# end
# function find_coderange(src, lineno)
#     clidx = find_codeloc(src, lineno)
#     ibegin = searchsortedfirst(src.codelocs, clidx)
#     ibegin += src.codelocs[ibegin] > lineno
#     iend = searchsortedlast(src.codelocs, clidx)
#     return ibegin:iend
# end

function sparam_name(mi::MethodInstance, i::Int)
    sig = (mi.def::Method).sig::UnionAll
    while true
        i == 1 && break
        sig = sig.body::UnionAll
        i -= 1
    end
    return sig.var.name
end

# # Get the name of the function being called
# function extract_call_name(stmt, mi)::Union{Symbol,Nothing}
#     stmt.head ∈ (:call, :invoke) || return nothing
#     f = stmt.args[1]
#     if isa(f, GlobalRef) && f.mod === Core && f.name == :_apply_iterate   # handle vararg calls
#         # Sanity check
#         fiter = stmt.args[2]
#         @assert isa(fiter, GlobalRef) && fiter.name == :iterate
#         f = stmt.args[3]   # get the actual call
#     end
#     if is_slot(f)
#         return src.slotnames[f.id]
#     end
#     if isa(f, SSAValue)
#         f = src.ssavaluetypes[f.id]
#         if isa(fname, Core.Const)
#             f = fname.val
#         end
#     end
#     if isexpr(f, :static_parameter)
#         return sparam_name(mi, f.args[1]::Int)
#     end
#     if isa(f, QuoteNode)
#         f = f.val
#     end
#     isa(f, GlobalRef) && return f.name
#     return nothing
# end

function getsrc(@nospecialize(f), @nospecialize(t))
    srcrts = code_typed(f, t; debuginfo=:source, optimize=false)
    return only(srcrts)
end

function is_function_def(node)  # this is not `Base.is_function_def`
    kind(node) == K"function" && return true
    kind(node) == K"=" && kind(child(node, 1)) ∈ KSet"call where" && return true
    return false
end

function get_function_def(rootnode)
    while kind(rootnode) == K"macrocall"
        idx = findlast(node -> is_function_def(node) || kind(node) == K"macrocall", children(rootnode))
        idx === nothing && break
        rootnode = child(rootnode, idx)
    end
    return rootnode
end

function collect_symbol_nodes!(symlocs::AbstractDict, node)
    kind(node) == K"->" && return symlocs     # skip inner functions (including `do` blocks below)
    is_function_def(node) && return symlocs
    if kind(node) == K"Identifier" || is_operator(node)
        name = node.val
        if isa(name, Symbol)
            locs = get!(Vector{typeof(node)}, symlocs, name)
            push!(locs, node)
        end
    end
    if is_literal(node) && node.val !== nothing    # FIXME: distinguish literal `nothing` from source `nothing`
        locs = get!(Vector{typeof(node)}, symlocs, node.val)
        push!(locs, node)
    end
    if haschildren(node)
        for c in (kind(node) == K"do" ? (child(node, 1),) : children(node))  # process only `g(args...)` in `g(args...) do ... end`
            collect_symbol_nodes!(symlocs, c)
        end
    end
    return symlocs
end

# Find all places in the source code where a symbol is used
function collect_symbol_nodes(rootnode)
    rootnode = get_function_def(rootnode)
    is_function_def(rootnode) || error("expected function definition, got ", sourcetext(rootnode))
    symlocs = Dict{Any,Vector{typeof(rootnode)}}()
    return collect_symbol_nodes!(symlocs, child(rootnode, 2))
end

function map_ssas_to_source(src, rootnode, Δline)
    # Find all leaf-nodes for a given symbol
    symlocs = collect_symbol_nodes(rootnode)      # symlocs = Dict(:name => [node1, node2, ...])
    # Initialize the type-assignment of each slot at each use location
    symtyps = IdDict{typeof(rootnode),Any}()                     # symtyps = IdDict(node => typ)
    # Initialize the (possibly ambiguous) attributions for each stmt in `src` (`stmt = src.code[i]`)
    mappings = [Union{SyntaxNode,TypedSyntaxNode}[] for _ in eachindex(src.code)]  # mappings[i] = [node1, node2, ...]

    # Append (to `mapped`) all nodes in `targets` that are consistent with the line number of the `i`th stmt
    function append_targets_for_line!(mapped, i, targets)
        j = src.codelocs[i]
        linerange = src.linetable[j].line + Δline : (
                    j < length(src.linetable) ? src.linetable[j+1].line - 1  + Δline : typemax(Int))
        for t in targets
            source_line(t) ∈ linerange && push!(mapped, t)
        end
        return mapped
    end
    # For a call argument `arg`, find all source statements that match
    function append_targets_for_arg!(mapped, i, arg)
        targets = if is_slot(arg)
            # If `arg` is a variable, e.g., the `x` in `f(x)`
            get(symlocs, src.slotnames[arg.id], nothing)  # find all places this variable is used
        elseif isa(arg, GlobalRef)
            get(symlocs, arg.name, nothing)  # find all places this name is used
        elseif isa(arg, SSAValue)
            # If `arg` is the result from a call, e.g., the `g(x)` in `f(g(x))`
            mappings[arg.id]
        elseif isa(arg, Core.Const)
            get(symlocs, arg.val, nothing)   # FIXME: distinguish this `nothing` from a literal `nothing`
        elseif is_src_literal(arg)
            get(symlocs, arg, nothing)   # FIXME: distinguish this `nothing` from a literal `nothing`
        end
        if targets !== nothing
            append_targets_for_line!(mapped, i, targets)        # select the subset consistent with the line number
        end
        return mapped
    end

    argmapping = typeof(rootnode)[]
    for (i, mapped, stmt) in zip(eachindex(mappings), mappings, src.code)
        empty!(argmapping)
        if is_slot(stmt) || isa(stmt, SSAValue)
            append_targets_for_arg!(mapped, i, stmt)
        elseif isa(stmt, Core.ReturnNode)
            append_targets_for_line!(mapped, i, append_targets_for_arg!(argmapping, i, stmt.val))
        elseif isa(stmt, Expr)
            if stmt.head == :(=)
                # We defer setting up `symtyps` for the LHS because processing the RHS first might eliminate ambiguities
                # # Update `symtyps` for this assignment
                lhs = stmt.args[1]
                @assert is_slot(lhs)
                # For `mappings` we're interested only in the right hand side of this assignment
                stmt = stmt.args[2]
                if is_slot(stmt) || isa(stmt, SSAValue)  # can we just look up the answer?
                    append_targets_for_arg!(mapped, i, stmt)
                    length(mapped) > 1 && filter!(mapped) do argnode
                        if kind(argnode.parent) == K"tuple"
                            is_prec_assignment(argnode.parent.parent) && any(==(argnode), children(child(argnode.parent.parent, 2)))
                        else
                            is_prec_assignment(argnode.parent) && argnode == child(argnode.parent, 2)  # rhs
                        end
                    end
                    if length(mapped) == 1
                        symtyps[only(mapped)] = is_slot(stmt) ? src.slottypes[stmt.id] : src.ssavaluetypes[stmt.id]
                    end
                    append_targets_for_arg!(argmapping, i, lhs)
                    filter!(argmapping) do argnode
                        if kind(argnode.parent) == K"tuple"
                            is_prec_assignment(argnode.parent.parent) && any(==(argnode), children(child(argnode.parent.parent, 1)))
                        else
                            is_prec_assignment(argnode.parent) && argnode == child(argnode.parent, 1)
                        end
                    end
                    if length(argmapping) == 1
                        symtyps[only(argmapping)] = src.ssavaluetypes[i]
                    end
                    empty!(argmapping)
                    continue
                end
                isa(stmt, Expr) || continue
                # The right hand side was an expression. Fall through to the generic `call` analysis.
            end
            if stmt.head == :call && is_indexed_iterate(stmt.args[1])
                id = stmt.args[2]
                @assert isa(id, SSAValue)
                append!(mapped, mappings[id.id])
                continue
            end
            # When analyzing calls, we start with the symbols. For any that have been attributed to one or more
            # nodes in the source, we make a consistency argument: which *parent* nodes take all of these as arguments?
            # In many cases this allows unique assignment.
            # Let's take a simple example: `x + sin(x + π / 4)`: in this case, `x + ` appears in two places but
            # you can disambiguate it by noting that `x + π / 4` only occurs in one place.
            # Note that the function name (e.g., `:sin`) is not special, we can effectively treat all as
            # `invoke(f, args...)` and consider `f` just like any other argument.
            # TODO?: handle gensymmed names, e.g., kw bodyfunctions?
            # The advantage of this approach is precision: we don't depend on ordering of statements,
            # so when it works you know you are correct.
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
            append!(mapped, stmtmapping)
            sort!(mapped; by=t->t.position)   # since they went into a set, best to order them within the source
            stmt = src.code[i]   # re-get the statement so we process slot-assignment
            if length(mapped) == 1 && isa(stmt, Expr)
                # We've mapped the call uniquely.
                # Final step: set up symtyps for all the user-visible variables
                # Because lowering can build methods that take a different number of arguments than appear in the
                # source text, don't try to count arguments. Instead, find a symbol that is part of
                # `node` or, for the LHS of a `slot = callexpr` statement, one that shares a parent with `node`.
                node = only(mapped)
                if stmt.head == :(=)
                    # Tag the LHS of this expression
                    arg = stmt.args[1]
                    @assert is_slot(arg)
                    sym = src.slotnames[arg.id]
                    if !isempty(string(sym))
                        lhsnode = node
                        while !is_prec_assignment(lhsnode)
                            lhsnode = lhsnode.parent
                        end
                        lhsnode = child(lhsnode, 1)
                        if kind(lhsnode) == K"tuple"   # tuple destructuring
                            found = false
                            for child in children(lhsnode)
                                if kind(child) == K"Identifier"
                                    if child.val == sym
                                        lhsnode = child
                                        found = true
                                        break
                                    end
                                end
                            end
                            @assert found
                        end
                        symtyps[lhsnode] = src.ssavaluetypes[i]
                    end
                    # Now process the RHS
                    stmt = stmt.args[2]
                end
                # Process the call expr
                if isa(stmt, Expr)
                    for arg in stmt.args
                        # For arguments that are slots, follow them backwards.
                        # (We're not assigning type to node, we're assigning nodes to ssavalues.)
                        # Arguments can locally be SSAValues but ultimately map back to slots
                        j = 0
                        while isa(arg, SSAValue)
                            j = arg.id
                            arg = src.ssavaluetypes[j]   # keep trying in case it maps back to a slot
                        end
                        if is_slot(arg)
                            sym = src.slotnames[arg.id]
                            sym == Symbol("") && continue
                            for t in symlocs[sym]
                                haskey(symtyps, t) && continue
                                if t.parent == node
                                    is_prec_assignment(node) && t == child(node, 1) && continue
                                    symtyps[t] = if j > 0
                                        src.ssavaluetypes[j]
                                    else
                                        # We failed to find it as an SSAValue, it must have type assigned at function entry
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
    return mappings, symtyps
end

function is_indexed_iterate(arg)
    isa(arg, GlobalRef) || return false
    arg.mod == Base || return false
    return arg.name == :indexed_iterate
end

is_slot(@nospecialize(arg)) = isa(arg, SlotNumber) || isa(arg, TypedSlot)

is_src_literal(x) = isa(x, Integer) || isa(x, AbstractFloat) || isa(x, String) || isa(x, Char) || isa(x, Symbol)
