
mutable struct TypedSyntaxData <: AbstractSyntaxData
    source::SourceFile
    typedsource::CodeInfo
    raw::GreenNode{SyntaxHead}
    position::Int
    val::Any
    typ::Any        # can either be a Type or `nothing`
    runtime::Bool   # true if this represents a call made by runtime dispatch (Cthulhu callsite annotation)
end
TypedSyntaxData(sd::SyntaxData, src::CodeInfo, typ=nothing) = TypedSyntaxData(sd.source, src, sd.raw, sd.position, sd.val, typ, false)

const TypedSyntaxNode = JuliaSyntax.TreeNode{TypedSyntaxData}
const MaybeTypedSyntaxNode = Union{SyntaxNode,TypedSyntaxNode}

struct NoDefaultValue end
const no_default_value = NoDefaultValue()

# These are TypedSyntaxNode constructor helpers
# Call these directly if you want both the TypedSyntaxNode and the `mappings` list,
# where `mappings[i]` corresponds to the list of nodes matching `(src::CodeInfo).code[i]`.
function tsn_and_mappings(@nospecialize(f), @nospecialize(t); kwargs...)
    m = which(f, t)
    src, rt = getsrc(f, t)
    tsn_and_mappings(m, src, rt; kwargs...)
end

function tsn_and_mappings(m::Method, src::CodeInfo, @nospecialize(rt); warn::Bool=true, strip_macros::Bool=false, kwargs...)
    def = definition(String, m)
    if isnothing(def)
        warn && @warn "couldn't retrieve source of $m"
        return nothing, nothing
    end
    return tsn_and_mappings(m, src, rt, def...; warn, strip_macros, kwargs...)
end

function tsn_and_mappings(m::Method, src::CodeInfo, @nospecialize(rt), sourcetext::AbstractString, lineno::Integer; warn::Bool=true, strip_macros::Bool=false, kwargs...)
    rootnode = JuliaSyntax.parsestmt(SyntaxNode, sourcetext; filename=functionloc(m)[1], first_line=lineno, kwargs...)
    if strip_macros
        rootnode = get_function_def(rootnode)
        if !is_function_def(rootnode)
            warn && @warn "couldn't retrieve source of $m"
            return nothing, nothing
        end
    end
    Δline = lineno - m.line   # offset from original line number (Revise)
    mappings, symtyps = map_ssas_to_source(src, rootnode, Δline)
    node = TypedSyntaxNode(rootnode, src, mappings, symtyps)
    node.typ = rt
    return node, mappings
end

TypedSyntaxNode(@nospecialize(f), @nospecialize(t); kwargs...) = tsn_and_mappings(f, t; kwargs...)[1]

function TypedSyntaxNode(mi::MethodInstance; kwargs...)
    m = mi.def::Method
    src, rt = getsrc(mi)
    tsn_and_mappings(m, src, rt; kwargs...)[1]
end

TypedSyntaxNode(rootnode::SyntaxNode, src::CodeInfo, Δline::Integer=0) =
    TypedSyntaxNode(rootnode, src, map_ssas_to_source(src, rootnode, Δline)...)

function TypedSyntaxNode(rootnode::SyntaxNode, src::CodeInfo, mappings, symtyps)
    # There may be ambiguous assignments back to the source; preserve just the unambiguous ones
    node2ssa = IdDict{SyntaxNode,Int}(only(list) => i for (i, list) in pairs(mappings) if length(list) == 1)
    # Copy `rootnode`, adding type annotations
    trootnode = TypedSyntaxNode(nothing, nothing, TypedSyntaxData(rootnode.data::SyntaxData, src, gettyp(node2ssa, rootnode, src)))
    addchildren!(trootnode, rootnode, src, node2ssa, symtyps, mappings)
    # Add argtyps to signature
    fnode = get_function_def(trootnode)
    if is_function_def(fnode)
        sig, body = children(fnode)
        map_signature!(sig, src)
    end
    return trootnode
end

# Recursive construction of the TypedSyntaxNode tree from the SyntaxNodeTree
function addchildren!(tparent, parent, src::CodeInfo, node2ssa, symtyps, mappings)
    if haschildren(parent) && tparent.children === nothing
        tparent.children = TypedSyntaxNode[]
    end
    for child in children(parent)
        tnode = TypedSyntaxNode(tparent, nothing, TypedSyntaxData(child.data::SyntaxData, src, gettyp(node2ssa, child, src)))
        if tnode.typ === nothing && (#=is_literal(child) ||=# kind(child) == K"Identifier")
            tnode.typ = get(symtyps, child, nothing)
        end
        push!(tparent, tnode)
        addchildren!(tnode, child, src, node2ssa, symtyps, mappings)
    end
    # In `return f(args..)`, copy any types assigned to `f(args...)` up to the `[return]` node
    if kind(tparent) == K"return" && haschildren(tparent)
        childs = children(tparent)
        tparent.typ = isempty(childs) ? Nothing : only(childs).typ
    end
    # Replace the entry in `mappings` to be the typed node
    i = get(node2ssa, parent, nothing)
    if i !== nothing
        @assert length(mappings[i]) == 1
        mappings[i][1] = tparent
    end
    return tparent
end

map_signature!(sig::TypedSyntaxNode, src::CodeInfo) =
    map_signature!(sig, src.slotnames::Vector{Symbol}, src.slottypes::Union{Nothing,Vector{Any}})

function map_signature!(sig::TypedSyntaxNode, slotnames::Vector{Symbol}, slottypes::Vector{Any})
    function argidentifier(arg)
        arg, defaultval = striparg(arg)
        if kind(arg) == K"::"
            nchildren = length(children(arg))
            nchildren == 1 && return nothing, defaultval
            @assert nchildren == 2
            arg = child(arg, 1)
        end
        if kind(arg) == K"$"
            return nothing, defaultval
        end
        if kind(arg) == K"."
            arg = last(children(arg))
            if kind(arg) == K"inert"
                @assert kind(only(children(arg))) == K"$"
                return nothing, defaultval
            end
            @assert kind(arg) == K"quote"
            arg = only(children(arg))
        end
        if kind(arg) == K"curly"
            arg = first(children(arg))
        end
        kind(arg) == K"tuple" && return nothing, defaultval     # FIXME? see extrema2 test
        @assert kind(arg) == K"Identifier" || is_operator(arg)
        return arg, defaultval
    end

    while kind(sig) ∈ KSet"where ::"   # handle MyType{T}(args...) and return-type annotations
        sig = child(sig, 1)
    end
    @assert kind(sig) == K"call"
    # First, match named args, since those have to be unique
    sigarg = fill(0, length(slotnames))
    slotarg = Int[]
    argcontainer = children(sig)   # we use a flexible container, because the [parameters] node (for kw-funcs) is its own container
    j = 1
    havekws = false
    while j <= length(argcontainer)
        arg = argcontainer[j]
        if kind(arg) == K"parameters"
            argcontainer = children(arg)
            j = 1
            arg = argcontainer[j]
            havekws = true
        end
        idx = nothing
        # Try to find an identifier for this arg
        arg, defaultval = argidentifier(arg)
        if arg !== nothing
            name = arg.val::Symbol
            idx = findfirst(==(name), slotnames)
            if idx === nothing && isempty(slotarg) && slotnames[1] == Symbol("#self#")
                # The first argument is the function name itself, and matches `#self#`
                idx = 1
            end
        end
        if idx !== nothing
            # Match made
            push!(slotarg, idx)
            sigarg[idx] = length(slotarg)
        else
            # No match, save an empty spot
            push!(slotarg, 0)
        end
        j += 1
    end
    if any(iszero, slotarg)
        # Some arguments went unmatched
        kwdivider = 1
        if havekws && slotnames[1] !== Symbol("#self#")
            kwdivider = findfirst(1:length(slotnames)) do i
                slotnames[i] == Symbol("") && unwrapinternal(slottypes[i]) <: Function  # this should be the parent function as an argument
            end
            if kwdivider === nothing
                kwdivider = 1
            end
            if length(slottypes) >= 2 && slotnames[2] == Symbol("") && (nt = unwrapinternal(slottypes[2])) <: NamedTuple
                # Match kwargs
                argcontainer = children(last(children(sig)))
                offset = length(children(sig)) - 1
                names, typs = (Base.unwrap_unionall(nt)::DataType).parameters
                if names isa Tuple{Vararg{Symbol}} && typs isa DataType && typs.name === Tuple.name
                    for j = 1 : length(argcontainer)
                        if iszero(slotarg[j + offset]) || slottypes[slotarg[j + offset]] === Union{}
                            arg = argcontainer[j]
                            arg, defaultval = argidentifier(arg)::Tuple{TypedSyntaxNode, Any}
                            name = arg.val::Symbol
                            i = findfirst(==(name), names)
                            if i !== nothing
                                arg.typ = typs.parameters[i]
                                slotarg[j + offset] = 0   # mark as complete
                            end
                        end
                    end
                end
            end
        end
        # Match the remaining positional arguments
        i = kwdivider + 1
        argcontainer = children(sig)
        for j = 2 : length(argcontainer)
            arg = argcontainer[j]
            kind(arg) == K"parameters" && break
            if iszero(slotarg[j])
                if i <= length(slotnames)
                    slotarg[j] = i
                    sigarg[i] = j
                end
            end
            i += 1
        end
    end
    argcontainer, offset = children(sig), 0
    for (j, idx) in enumerate(slotarg)
        arg, defaultval = striparg(argcontainer[j - offset])
        if kind(arg) == K"parameters"
            offset = length(argcontainer) - 1
            argcontainer = children(arg)
            arg, defaultval = striparg(argcontainer[j - offset])
        end
        idx == 0 && continue
        if kind(arg) == K"::" && length(children(arg)) == 2
            arg = child(arg, 1)
        end
        arg.typ = unwrapinternal(slottypes[idx])
    end

    # It's annoying to print the signature as `foo::typeof(foo)(a::Int)`
    # Strip the type annotation from the function name
    arg, _ = argidentifier(child(sig, 1))
    if arg !== nothing
        arg.typ = nothing
    end
    return sig
end
map_signature!(sig::TypedSyntaxNode, slotnames, slottypes::Nothing) = nothing

# For a signature argument, strip "decorations"
function striparg(arg)
    defaultval = no_default_value
    while kind(arg) ∈ KSet"... = macrocall"
        if kind(arg) == K"..."
            arg = only(children(arg))
        end
        if kind(arg) == K"="
            defaultval = child(arg, 2)
            arg = child(arg, 1)
        end
        if kind(arg) == K"macrocall"
            arg = last(children(arg))    # FIXME: is the variable always the final argument?
        end
    end
    return arg, defaultval
end

function unwrapinternal(@nospecialize(T))
    isa(T, Core.Const) && return Core.Typeof(T.val)
    isa(T, Core.PartialStruct) && return T.typ
    return T
end
function gettyp(node2ssa, node, src)
    i = get(node2ssa, node, nothing)
    i === nothing && return nothing
    stmt = src.code[i]
    ssavaluetypes = src.ssavaluetypes::Vector{Any}
    if isa(stmt, Core.ReturnNode)
        arg = stmt.val
        isa(arg, SSAValue) && return unwrapinternal(ssavaluetypes[arg.id])
        is_slot(arg) && return unwrapinternal((src.slottypes::Vector{Any})[arg.id])
    end
    return unwrapinternal(ssavaluetypes[i])
end

Base.copy(tsd::TypedSyntaxData) = TypedSyntaxData(tsd.source, tsd.typedsource, tsd.raw, tsd.position, tsd.val, tsd.typ, tsd.runtime)

gettyp(node::AbstractSyntaxNode) = gettyp(node.data)
gettyp(::Nothing) = nothing
gettyp(::JuliaSyntax.SyntaxData) = nothing
gettyp(data::TypedSyntaxData) = data.typ

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

function getsrc(mi::MethodInstance)
    cis = Base.code_typed_by_type(mi.specTypes; debuginfo=:source, optimize=false)
    isempty(cis) && error("no applicable type-inferred code found for ", mi)
    length(cis) == 1 || error("got $(length(cis)) possible type-inferred results for ", mi,
                              ", you may need a more specialized signature")
    return cis[1]::Pair{CodeInfo}
end

function is_function_def(node)  # this is not `Base.is_function_def`
    kind(node) == K"function" && return true
    if kind(node) == K"=" && length(children(node)) >= 1
        sig = child(node, 1)
        while(kind(sig) ∈ KSet"where ::")   # allow MyType{T}(args...) and return-type annotations
            sig = child(sig, 1)
        end
        kind(sig) == K"call" && return true
    end
    return false
end

# Strip macros and return the function-definition node
function get_function_def(rootnode)
    while kind(rootnode) ∈ KSet"macrocall global local const"
        idx = findlast(node -> is_function_def(node) || kind(node) == K"macrocall", children(rootnode))
        idx === nothing && break
        rootnode = child(rootnode, idx)
    end
    return rootnode
end

function num_positional_args(tsn::AbstractSyntaxNode)
    TypedSyntax.is_function_def(tsn) || return 0
    sig, _ = children(tsn)
    while kind(sig) ∈ KSet"where ::"
        sig = child(sig, 1)
    end
    @assert kind(sig) == K"call"
    for (i, node) in enumerate(children(sig))
        kind(node) == K"parameters" && return i-1
    end
    return length(children(sig))
end

# Recursively traverse `rootnode` and its children, and put all the instances in which
# a given symbol appears into `symlocs[val]`.
# This includes function names, operators, and literal values (like `1`, `"hello"`, etc.)
# These will be used to do argument-matching in calls.
function collect_symbol_nodes(rootnode)
    rootnode = get_function_def(rootnode)
    is_function_def(rootnode) || error("expected function definition, got ", sourcetext(rootnode))
    symlocs = Dict{Any,Vector{typeof(rootnode)}}()
    return collect_symbol_nodes!(symlocs, child(rootnode, 2))
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
        if kind(node) == K"do"
            # process only `g(args...)` in `g(args...) do ... end`
            collect_symbol_nodes!(symlocs, child(node, 1))
        elseif kind(node) == K"generator"
            for c in Iterators.drop(children(node), 1)
                collect_symbol_nodes!(symlocs, c)
            end
        else
            for c in children(node)
                collect_symbol_nodes!(symlocs, c)
            end
        end
    end
    return symlocs
end

# Main logic for mapping `src.code[i]` to node(s) in the SyntaxNode tree
# Success: when we map it to a unique node
# Δline is the (Revise) offset of the line number
function map_ssas_to_source(src::CodeInfo, rootnode::SyntaxNode, Δline::Int)
    mi = src.parent::MethodInstance
    slottypes = src.slottypes::Union{Nothing, Vector{Any}}
    have_slottypes = slottypes !== nothing
    ssavaluetypes = src.ssavaluetypes::Vector{Any}
    # Find all leaf-nodes for a given symbol
    symlocs = collect_symbol_nodes(rootnode)      # symlocs = Dict(:name => [node1, node2, ...])
    # Initialize the type-assignment of each slot at each use location
    symtyps = IdDict{typeof(rootnode),Any}()                     # symtyps = IdDict(node => typ)
    # Initialize the (possibly ambiguous) attributions for each stmt in `src` (`stmt = src.code[i]`)
    mappings = [MaybeTypedSyntaxNode[] for _ in eachindex(src.code)]  # mappings[i] = [node1, node2, ...]

    used = BitSet()
    for (i, stmt) in enumerate(src.code)
        Core.Compiler.scan_ssa_use!(push!, used, stmt)
        if isa(stmt, Core.ReturnNode)
            val = stmt.val
            if isa(val, SSAValue)
                push!(used, val.id)
            elseif isa(val, SlotNumber)
                push!(used, i)
            end
        elseif isexpr(stmt, :(=))
            push!(used, i)
        end
    end

    # Append (to `mapped`) all nodes in `targets` that are consistent with the line number of the `i`th stmt
    # (Essentially `copy!(mapped, filter(predicate, targets))`)
    function append_targets_for_line!(mapped#=::Vector{nodes}=#, i::Int, targets#=::Vector{nodes}=#)
        j = src.codelocs[i]
        lt = src.linetable::Vector{Any}
        start = getline(lt, j) + Δline
        stop = getnextline(lt, j, Δline) - 1
        linerange = start : stop
        for t in targets
            source_line(t) ∈ linerange && push!(mapped, t)
        end
        return mapped
    end
    # For a call argument `arg`, find all source statements that match
    function append_targets_for_arg!(mapped#=::Vector{nodes}=#, i::Int, @nospecialize(arg))
        targets = get_targets(arg)::Union{Nothing,Vector{<:MaybeTypedSyntaxNode}}
        if targets !== nothing
            append_targets_for_line!(mapped, i, targets)        # select the subset consistent with the line number
        end
        return mapped
    end
    function get_targets(@nospecialize(arg))
        return if is_slot(arg)
            # If `arg` is a variable, e.g., the `x` in `f(x)`
            name = src.slotnames[(arg::Union{SlotNumber,TypedSlot}).id]
            is_gensym(name) ? nothing : get(symlocs, symloc_key(name), nothing)
            # get(symlocs, src.slotnames[arg.id], nothing)  # find all places this variable is used
        elseif isa(arg, GlobalRef)
            get(symlocs, arg.name, nothing)  # find all places this name is used
        elseif isa(arg, SSAValue)
            # If `arg` is the result from a call, e.g., the `g(x)` in `f(g(x))`
            mappings[arg.id]
        elseif isa(arg, Core.Const)
            get(symlocs, arg.val, nothing)   # FIXME: distinguish this `nothing` from a literal `nothing`
        elseif isa(arg, QuoteNode)
            get(symlocs, arg.value, nothing)
        elseif is_src_literal(arg)
            get(symlocs, arg, nothing)   # FIXME: distinguish this `nothing` from a literal `nothing`
        elseif isexpr(arg, :static_parameter)
            name = sparam_name(mi, arg.args[1]::Int)
            get(symlocs, name, nothing)
        else
            nothing
        end
    end
    # This is used in matching nodes to `:=` stmts.
    # In cases where there is more than one match, let's try to eliminate some of them.
    # We know this stmt is an assignment, so look for a parent node that is an assignment
    # and for which argnode is in the correct side of the assignment
    function filter_assignment_targets!(targets, is_rhs::Bool)
        length(targets) > 1 && filter!(targets) do argnode
            p = argnode.parent::MaybeTypedSyntaxNode
            if kind(p) == K"tuple"  # tuple-structuring (go up one more node to see if it's an assignment)
                argnode = p
                p = argnode.parent::MaybeTypedSyntaxNode
            end
            is_prec_assignment(p) && argnode == child(p, 1 + is_rhs)  # is it the correct side of an assignment?
        end
        return targets
    end

    argmapping = typeof(rootnode)[]   # temporary storage
    for (i, mapped, stmt) in zip(eachindex(mappings), mappings, src.code)
        empty!(argmapping)
        if is_slot(stmt) || isa(stmt, SSAValue)
            append_targets_for_arg!(mapped, i, stmt)
        elseif isa(stmt, Core.ReturnNode)
            append_targets_for_line!(mapped, i, append_targets_for_arg!(argmapping, i, stmt.val))
        elseif isa(stmt, Expr)
            if stmt.head == :(=) && is_slot(stmt.args[1])
                # We defer setting up `symtyps` for the LHS because processing the RHS first might eliminate ambiguities
                # # Update `symtyps` for this assignment
                lhs = stmt.args[1]
                # For `mappings` we're interested only in the right hand side of this assignment
                stmt = stmt.args[2]
                if is_slot(stmt) || isa(stmt, SSAValue) || is_src_literal(stmt) # generic calls are handled below. Here, can we just look up the answer?
                    append_targets_for_arg!(mapped, i, stmt)
                    filter_assignment_targets!(mapped, true)   # match the RHS of assignments
                    if length(mapped) == 1
                        symtyps[only(mapped)] = unwrapinternal(
                                                (is_slot(stmt) & have_slottypes) ? slottypes[(stmt::Union{SlotNumber,TypedSlot}).id] :
                                                isa(stmt, SSAValue) ? ssavaluetypes[stmt.id] : #=literal=#typeof(stmt)
                        )
                    end
                    # Now try to assign types to the LHS of the assignment
                    append_targets_for_arg!(argmapping, i, lhs)
                    filter_assignment_targets!(argmapping, false)  # match the LHS of assignments
                    if length(argmapping) == 1
                        T = unwrapinternal(ssavaluetypes[i])
                        symtyps[only(argmapping)] = T
                    end
                    empty!(argmapping)
                    continue
                end
                isa(stmt, Expr) || continue
                # The right hand side was an expression. Fall through to the generic `call` analysis.
            end
            if stmt.head == :call && is_indexed_iterate(stmt.args[1])
                id = stmt.args[2]
                if isa(id, SSAValue)
                    append!(mapped, mappings[id.id])
                    continue
                elseif is_slot(id)
                    targets = get_targets(id)
                    targets !== nothing && append!(mapped, targets)
                    continue
                end
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
            for (iarg, _arg) in enumerate(stmt.args)
                args = if is_apply_iterate(stmt) && iarg >= 4 && isa(_arg, SSAValue) && is_tuple_stmt(src.code[_arg.id])
                    # In vararg (_apply_iterate) calls, any non-`...` args are bundled in a tuple.
                    # Split the tuple to extract the complete argument list.
                    tuplestmt = src.code[_arg.id]::Expr
                    tuplestmt.args[2:end]
                else
                    (_arg,)
                end
                for arg in args
                    # Collect all source-nodes that use this argument
                    append_targets_for_arg!(argmapping, i, arg)
                    if !isempty(argmapping)
                        if isempty(stmtmapping)
                            # First matched argument
                            # For each candidate source-node, push its parent-node into `stmtmapping`.
                            # The true call-node should be among these.
                            foreach(argmapping) do t
                                if t.parent !== nothing
                                    push!(stmtmapping, skipped_parent(t))
                                end
                            end
                        else
                            # Second or later matched argument
                            # A matching caller node needs to use all `stmt.args`,
                            # so we `intersect` to find the common node(s)
                            intersect!(stmtmapping, map(skipped_parent, argmapping))
                        end
                    end
                    empty!(argmapping)
                end
            end
            # Varargs require special handling because lowering modifies the call sequence heavily
            # Wait to map them until we get to the _apply_iterate statement
            if is_tuple_stmt(stmt)
                ii = i
                while ii < length(src.code) && is_tuple_stmt(src.code[ii+1])
                    ii += 1    # _apply_iterate can have multiple preceeding tuple statements
                end
                if ii < length(src.code) && is_apply_iterate(src.code[ii+1])
                    empty!(stmtmapping)   # block any possibility of matching
                end
            end
            append!(mapped, stmtmapping)
            sort!(mapped; by=t->t.position)   # since they went into a set, best to order them within the source
            rhs = stmt
            stmt = src.code[i]   # re-get the statement so we process slot-assignment
            if length(mapped) == 1 && isa(stmt, Expr)
                # We've mapped the call uniquely (i.e., we found the right match)
                node = only(mapped)
                # Handle some special cases where lowering modifies the user-code extensively
                if isexpr(rhs, :call) && (f = rhs.args[1]; isa(f, GlobalRef) && f.mod == Base && f.name == :Generator)
                    # Generator calls
                    pnode = node.parent
                    if pnode !== nothing
                        ppnode = pnode.parent
                        if kind(pnode) == K"generator" && ppnode !== nothing && kind(ppnode) == K"comprehension"
                            mapped[1] = node = pnode
                        end
                    end
                end
                if kind(node) == K"dotcall" && isexpr(rhs, :call) &&  (f = rhs.args[1]; isa(f, GlobalRef) && f.mod == Base && f.name == :broadcasted)
                    # Broadcasting: move the match to the `materialize` call
                    inext = i
                    while inext < length(src.code)
                        nextstmt = src.code[inext+=1]
                        if isexpr(nextstmt, :(=))
                            nextstmt = nextstmt.args[2]
                        end
                        if isexpr(nextstmt, :call)
                            f = nextstmt.args[1]
                            if isa(f, GlobalRef) && f.mod == Base && f.name == :broadcasted
                                empty!(mapped)
                                break
                            elseif isa(f, GlobalRef) && f.mod == Base && f.name == :materialize && nextstmt.args[2] === SSAValue(i)
                                push!(mappings[inext], node)
                                empty!(mapped)
                                break
                            end
                        end
                    end
                end
                # Final step: set up symtyps for all the user-visible variables
                # Because lowering can build methods that take a different number of arguments than appear in the
                # source text, don't try to count arguments. Instead, find a symbol that is part of
                # `node` or, for the LHS of a `slot = callexpr` statement, one that shares a parent with `node`.
                if stmt.head == :(=) && is_slot(stmt.args[1])
                    # Tag the LHS of this expression
                    arg = stmt.args[1]::Union{SlotNumber, TypedSlot}
                    sym = src.slotnames[arg.id]
                    if !is_gensym(sym)
                        lhsnode = node
                        while !is_prec_assignment(lhsnode) && (lhsparent = lhsnode.parent; lhsparent !== nothing)
                            lhsnode = lhsparent
                        end
                        lhsnode, found = find_identifier_or_tuplechild(child(lhsnode, 1), sym)
                        if found
                            symtyps[lhsnode] = ssavaluetypes[i]
                        end
                    end
                    # Now process the RHS
                    stmt = stmt.args[2]
                end
                # Process the call expr
                if isa(stmt, Expr)
                    for (iarg, _arg) in enumerate(stmt.args)
                        # For arguments that are slots, follow them backwards.
                        # (We're not assigning type to node, we're assigning nodes to ssavalues.)
                        # Arguments can locally be SSAValues but ultimately map back to slots
                        _arg, j = follow_back(src, _arg)
                        argjs = if is_apply_iterate(stmt) && iarg >= 4 && j > 0 && is_tuple_stmt(src.code[j])
                            # Split the non-va types in the call to _apply_iterate
                            tuplestmt = src.code[j]::Expr
                            Tuple{Any,Int}[follow_back(src, arg) for arg in tuplestmt.args[2:end]]
                        else
                            Tuple{Any,Int}[(_arg, j)]
                        end
                        for (arg, j) in argjs
                            if is_slot(arg)
                                arg = arg::Union{SlotNumber, TypedSlot}
                                sym = src.slotnames[arg.id]
                                itr = get(symlocs, symloc_key(sym), nothing)
                                itr === nothing && continue
                                for t in itr
                                    haskey(symtyps, t) && continue
                                    if skipped_parent(t) == node
                                        is_prec_assignment(node) && t == child(node, 1) && continue
                                        symtyps[t] = unwrapinternal(if j > 0
                                            ssavaluetypes[j]
                                        elseif have_slottypes
                                            # We failed to find it as an SSAValue, it must have type assigned at function entry
                                            slottypes[arg.id]
                                        else
                                            nothing
                                        end)
                                        break
                                    end
                                end
                            elseif isexpr(arg, :static_parameter)
                                id = arg.args[1]::Int
                                name = sparam_name(mi, id)
                                for t in symlocs[name]
                                    T = mi.sparam_vals[id]
                                    if Base.isvarargtype(T) || isa(T, TypeVar)
                                        symtyps[t] = T
                                    elseif T isa DataType
                                        symtyps[t] = Type{T}
                                    else
                                        symtyps[t] = Core.Compiler.Const(T)
                                    end
                                end
                            elseif isa(arg, GlobalRef)
                                T = nothing
                                if isconst(arg.mod, arg.name)
                                    T = Core.Typeof(getfield(arg.mod, arg.name))
                                    if T <: Function
                                        continue # it's confusing to annotate `zero(x)` as `zero::typeof(zero)(...)`
                                    end
                                else
                                    T = Any
                                end
                                for t in get(symlocs, arg.name, ())
                                    symtyps[t] = T
                                end
                            end
                        end
                    end
                end
            elseif isempty(mapped) && isexpr(stmt, :(=))
                lhs = stmt.args[1]
                if is_slot(lhs)
                    empty!(argmapping)
                    append_targets_for_arg!(argmapping, i, lhs)
                    if length(argmapping) == 1
                        node = only(argmapping)
                        mappings[i] = [node]
                    end
                end
            end
        end
        i ∈ used || empty!(mappings[i])   # if the result of the call is not used, don't attach a type to it
    end
    # Annotate literals
    for (item, nodes) in symlocs
        if is_src_literal(item)
            for node in nodes
                is_literal(node) || continue
                symtyps[node] = Core.Typeof(item)
            end
        end
    end
    return mappings, symtyps
end
map_ssas_to_source(src::CodeInfo, rootnode::SyntaxNode, Δline::Integer) = map_ssas_to_source(src, rootnode, Int(Δline))

function follow_back(src, arg)
    # Follow SSAValue backward to see if it maps back to a slot
    j = 0
    while isa(arg, SSAValue)
        j = arg.id
        arg = (src.ssavaluetypes::Vector{Any})[j]
    end
    return arg, j
end

function follow_back(src, arg, mappings)
    # Follow SSAValue backward to see if it maps back to a slot
    j = 0
    while isa(arg, SSAValue)
        j = arg.id
        arg = src.ssavaluetypes[j]
    end
    return arg, j
end

function is_indexed_iterate(arg)
    isa(arg, GlobalRef) || return false
    arg.mod == Base || return false
    return arg.name == :indexed_iterate
end

is_slot(@nospecialize(arg)) = isa(arg, SlotNumber) || isa(arg, TypedSlot)

is_src_literal(x) = isa(x, Integer) || isa(x, AbstractFloat) || isa(x, String) || isa(x, Char) || isa(x, Symbol)

function is_va_call(node::SyntaxNode)
    kind(node) == K"call" || return false
    for arg in children(node)
        kind(arg) == K"..." && return true
    end
    return false
end

function skipped_parent(node::SyntaxNode)
    pnode = node.parent
    pnode === nothing && return node
    ppnode = pnode.parent
    if ppnode !== nothing && kind(pnode) ∈ KSet"... quote"   # might need to add more things here
        return ppnode
    end
    return pnode
end

function is_apply_iterate(@nospecialize(stmt))
    isexpr(stmt, :call) || return false
    f = stmt.args[1]
    return isa(f, GlobalRef) && f.mod === Core && f.name == :_apply_iterate
end

function is_tuple_stmt(@nospecialize(stmt))
    isexpr(stmt, :call) || return false
    f = stmt.args[1]
    return isa(f, GlobalRef) && f.mod === Core && f.name == :tuple
end

is_gensym(name::Symbol) = name == Symbol("") || string(name)[1] == '#'

is_runtime(node::TypedSyntaxNode) = node.runtime
is_runtime(::AbstractSyntaxNode) = false

function symloc_key(sym::Symbol)
    ssym = string(sym)
    endswith(ssym, "...") && return Symbol(ssym[1:end-3])
    return sym
end

function getline(lt, j)
    linfo = (j == 0 ? first(lt) : lt[j])::Core.LineInfoNode
    linfo.inlined_at == 0 && return linfo.line
    @assert linfo.method === Symbol("macro expansion")
    linfo = lt[linfo.inlined_at]::Core.LineInfoNode
    return linfo.line
end

function getnextline(lt, j, Δline)
    j == 0 && return typemax(Int)
    j += 1
    while j <= length(lt)
        linfo = lt[j]::Core.LineInfoNode
        linfo.inlined_at == 0 && return linfo.line + Δline
        j += 1
    end
    return typemax(Int)
end

function find_identifier_or_tuplechild(node::AbstractSyntaxNode, sym)
    kind(node) == K"Identifier" && node.val === sym && return node, true
    if kind(node) == K"tuple"   # tuple destructuring
        for child in children(node)
            tmpnode, found = find_identifier_or_tuplechild(child, sym)
            found && return tmpnode, found
        end
    end
    return node, false
end
