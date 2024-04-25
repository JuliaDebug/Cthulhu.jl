highlighter_exists(config::CthulhuConfig) =
    Sys.which(config.highlighter.exec[1]) !== nothing

function highlight(io, x, lexer, config::CthulhuConfig)
    _print = endswith(x, '\n') ? print : println
    config.enable_highlighter || return _print(io, x)
    if lexer == "llvm"
        InteractiveUtils.print_llvm(io, x)
    elseif lexer == "asm"
        InteractiveUtils.print_native(io, x)
    else
        if !highlighter_exists(config)
            @warn "Highlighter command $(config.highlighter.exec[1]) does not exist."
            return _print(io, x)
        end
        cmd = `$(config.highlighter) $lexer`
        open(pipeline(cmd; stdout=io, stderr=stderr), "w") do io
            _print(io, x)
        end
    end
end

@static if VERSION < v"1.12.0-DEV.669"
    function cthulhu_llvm(io::IO, mi, optimize::Bool, debuginfo, world::UInt,
                          config::CthulhuConfig, dump_module::Bool=false, raw::Bool=false)
        dump = InteractiveUtils._dump_function_llvm(
            mi, world,
            #=wrapper=# false, !raw,
            dump_module, optimize, debuginfo != DInfo.none ? :source : :none,
            Base.CodegenParams())
        highlight(io, dump, "llvm", config)
    end

    function cthulhu_native(io::IO, mi, ::Bool, debuginfo, world::UInt,
                            config::CthulhuConfig, dump_module::Bool=false, raw::Bool=false)
        if dump_module
            dump = InteractiveUtils._dump_function_native_assembly(
                mi, world,
                #=wrapper=# false, #=syntax=# config.asm_syntax,
                debuginfo != DInfo.none ? :source : :none,
                #=binary=# false, raw,
                Base.CodegenParams())
        else
            dump = InteractiveUtils._dump_function_native_disassembly(
                mi, world,
                #=wrapper=# false, #=syntax=# config.asm_syntax,
                debuginfo != DInfo.none ? :source : :none,
                #=binary=# false)
        end
        highlight(io, dump, "asm", config)
    end
else
    function cthulhu_llvm(io::IO, mi, src::CodeInfo, optimize::Bool, debuginfo, world::UInt,
                          config::CthulhuConfig, dump_module::Bool=false, raw::Bool=false)
        dump = InteractiveUtils._dump_function_llvm(
            mi, src,
            #=wrapper=# false, !raw,
            dump_module, optimize, debuginfo != DInfo.none ? :source : :none,
            Base.CodegenParams())
        highlight(io, dump, "llvm", config)
    end

    function cthulhu_native(io::IO, mi, src::CodeInfo, ::Bool, debuginfo, world::UInt,
                            config::CthulhuConfig, dump_module::Bool=false, raw::Bool=false)
        if dump_module
            dump = InteractiveUtils._dump_function_native_assembly(
                mi, src,
                #=wrapper=# false, #=syntax=# config.asm_syntax,
                debuginfo != DInfo.none ? :source : :none,
                #=binary=# false, raw,
                Base.CodegenParams())
        else
            dump = InteractiveUtils._dump_function_native_disassembly(
                mi, world,
                #=wrapper=# false, #=syntax=# config.asm_syntax,
                debuginfo != DInfo.none ? :source : :none,
                #=binary=# false)
        end
        highlight(io, dump, "asm", config)
    end

    function cthulhu_ast(io::IO, mi, ::CodeInfo, optimize::Bool, debuginfo, world::UInt, config::CthulhuConfig)
        return cthulhu_ast(io, mi, optimize, debuginfo, world, config)
    end
end

function cthulhu_ast(io::IO, mi, ::Bool, debuginfo, ::UInt, config::CthulhuConfig)
    meth = mi.def::Method
    ast = definition(Expr, meth)
    if ast!==nothing
        if !config.pretty_ast
            dump(io, ast; maxdepth=typemax(Int))
        else
            show(io, ast)
            # Meta.show_sexpr(io, ast)
            # Could even highlight the above as some kind-of LISP
        end
    else
        @warn "Could not retrieve AST of $meth. AST display requires Revise.jl to be loaded."
    end
end

const __debuginfo = merge(IRShow.__debuginfo, Dict(
    :compact => function (src)
        src isa CodeInfo ? IRShow.__debuginfo[:source](src) : IRShow.inline_linfo_printer(src)
    end))

function is_type_unstable(code::Union{IRCode, CodeInfo}, idx::Int, used::BitSet)
    stmt = IRShow._stmt(code, idx)
    type = IRShow._type(code, idx)
    IRShow.should_print_ssa_type(stmt) || return false
    # `used` only contains used SSA values and ignores slots
    in_use = in(idx, used) || Meta.isexpr(stmt, :(=))
    return in_use && is_type_unstable(type)
end
is_type_unstable(@nospecialize(type)) = type isa Type && (!Base.isdispatchelem(type) || type == Core.Box)

cthulhu_warntype(args...; kwargs...) = cthulhu_warntype(stdout::IO, args...; kwargs...)
function cthulhu_warntype(io::IO, debuginfo::AnyDebugInfo,
    src::Union{CodeInfo,IRCode}, @nospecialize(rt), effects::Effects, mi::Union{Nothing,MethodInstance}=nothing;
    hide_type_stable::Bool=false, inline_cost::Bool=false, optimize::Bool=false,
    interp::CthulhuInterpreter=CthulhuInterpreter())
    if inline_cost
        isa(mi, MethodInstance) || error("Need a MethodInstance to show inlining costs. Call `cthulhu_typed` directly instead.")
    end
    cthulhu_typed(io, debuginfo, src, rt, nothing, effects, mi; iswarn=true, optimize, hide_type_stable, inline_cost, interp)
    return nothing
end

# # for API consistency with the others
# function cthulhu_typed(io::IO, mi::MethodInstance, optimize, debuginfo, params, config::CthulhuConfig)
#     interp = mkinterp(mi)
#     (; src, rt, infos, slottypes) = lookup(interp, mi, optimize)
#     ci = Cthulhu.preprocess_ci!(src, mi, optimize, config)
#     cthulhu_typed(io, debuginfo, src, rt, mi)
# end

cthulhu_typed(io::IO, debuginfo::DebugInfo, args...; kwargs...) =
    cthulhu_typed(io, Symbol(debuginfo), args...; kwargs...)
function cthulhu_typed(io::IO, debuginfo::Symbol,
    src::Union{CodeInfo,IRCode}, @nospecialize(rt), @nospecialize(exct),
    effects::Effects, mi::Union{Nothing,MethodInstance};
    iswarn::Bool=false, hide_type_stable::Bool=false, optimize::Bool=true,
    pc2remarks::Union{Nothing,PC2Remarks}=nothing,
    pc2effects::Union{Nothing,PC2Effects}=nothing,
    pc2excts::Union{Nothing,PC2Excts}=nothing,
    inline_cost::Bool=false, type_annotations::Bool=true, annotate_source::Bool=false,
    inlay_types_vscode::Bool=false, diagnostics_vscode::Bool=false, jump_always::Bool=false,
    interp::AbstractInterpreter=CthulhuInterpreter())

    debuginfo = IRShow.debuginfo(debuginfo)
    lineprinter = __debuginfo[debuginfo]
    rettype = ignorelimited(rt)
    maxtypedepth = CONFIG.type_depth_limit
    lambda_io = IOContext(io, :limit=>true, :maxdepth => maxtypedepth)

    if annotate_source && isa(src, CodeInfo)
        tsn, _ = get_typed_sourcetext(mi, src, rt)
        if tsn !== nothing
            sig, body = children(tsn)
            # We empty the body when filling kwargs
            istruncated = isempty(children(body))
            idxend = istruncated ? JuliaSyntax.last_byte(sig) : lastindex(tsn.source)
            if src.slottypes === nothing
                @warn "Inference terminated in an incomplete state due to argument-type changes during recursion"
            end

            diagnostics_vscode &= iswarn # If warnings are off then no diagnostics are shown
            # Check if diagnostics are avaiable and if mi is defined in a file
            if !TypedSyntax.diagnostics_available_vscode() || isnothing(functionloc(mi)[1])
                diagnostics_vscode = false
            end
            if !TypedSyntax.inlay_hints_available_vscode() || isnothing(functionloc(mi)[1])
                inlay_types_vscode = false
            end

            vscode_io = IOContext(
                jump_always && inlay_types_vscode ? devnull : lambda_io,
                :inlay_hints => inlay_types_vscode ? Dict{String,Vector{TypedSyntax.InlayHint}}() : nothing ,
                :diagnostics => diagnostics_vscode ? TypedSyntax.Diagnostic[] : nothing
            )

            if istruncated
                printstyled(lambda_io, tsn; type_annotations, iswarn, hide_type_stable, idxend, maxtypedepth)
            else
                printstyled(vscode_io, tsn; type_annotations, iswarn, hide_type_stable, idxend, maxtypedepth)
            end

            callsite_diagnostics = TypedSyntax.Diagnostic[]
            if (diagnostics_vscode || inlay_types_vscode)
                vscode_io = IOContext(devnull, :inlay_hints=>vscode_io[:inlay_hints], :diagnostics=>vscode_io[:diagnostics])
                callsite_mis = Dict() # type annotation is a bit long so I skipped it, doesn't seem to affect performance
                visited_mis = Set{MethodInstance}((mi,))
                add_callsites!(callsite_mis, visited_mis, callsite_diagnostics, mi; optimize, annotate_source, interp)
                for callsite in values(callsite_mis)
                    if !isnothing(callsite)
                        descend_into_callsite!(vscode_io, callsite.tsn; iswarn, hide_type_stable, type_annotations)
                    end
                end
            end

            !isnothing(vscode_io[:diagnostics]) && append!(callsite_diagnostics, vscode_io[:diagnostics])
            TypedSyntax.display_diagnostics_vscode(callsite_diagnostics)
            TypedSyntax.display_inlay_hints_vscode(vscode_io)

            (jump_always && inlay_types_vscode) || println(lambda_io)
            istruncated && @info "This method only fills in default arguments; descend into the body method to see the full source."
            return nothing
        end
    end

    if isa(src, CodeInfo)
        # we're working on pre-optimization state, need to ignore `LimitedAccuracy`
        src = copy(src)
        src.ssavaluetypes = mapany(ignorelimited, src.ssavaluetypes::Vector{Any})

        if src.slotnames !== nothing
            slotnames = Base.sourceinfo_slotnames(src)
            lambda_io = IOContext(lambda_io, :SOURCE_SLOTNAMES => slotnames)
            show_variables(io, src, slotnames)
        end
    end
    maxtypedepth = CONFIG.type_depth_limit

    # preprinter configuration
    preprinter = if inline_cost
        isa(mi, MethodInstance) || throw("`mi::MethodInstance` is required")
        if isa(src, IRCode)
            @static if VERSION < v"1.11.0-DEV.258"
                code = src.stmts.inst
            else
                code = src.stmts.stmt
            end
        else
            code = src.code
        end
        cst = Vector{Int}(undef, length(code))
        params = CC.OptimizationParams(interp)
        sparams = CC.VarState[CC.VarState(sparam, false) for sparam in mi.sparam_vals]
        @static if VERSION ≥ v"1.11.0-DEV.32"
            CC.statement_costs!(cst, code, src, sparams, params)
        else
            CC.statement_costs!(cst, code, src, sparams, false, params)
        end
        total_cost = sum(cst)
        nd = ndigits(total_cost)
        _lineprinter = lineprinter(src)
        function (io, linestart, idx)
            str = idx > 0   ? lpad(cst[idx], nd+1) :
                  idx == -1 ? lpad(total_cost, nd+1) :
                  " "^(nd+1)
            str = sprint(; context=:color=>true) do @nospecialize io
                printstyled(io, str; color=:green, maxtypedepth)
            end
            if debuginfo === :source
                str *= " "
                linestart *= " "^(nd+2)
            end
            idx == -1 && (idx = 0) # fix up the special index for the default preprinter
            return str * _lineprinter(io, linestart, idx)
        end
    else
        _lineprinter = lineprinter(src)
        function (io, linestart, idx)
            idx == -1 && (idx = 0) # fix up the special index for the default preprinter
            _lineprinter(io, linestart, idx)
        end
    end
    # postprinter configuration
    ___postprinter = if type_annotations
        iswarn ? InteractiveUtils.warntype_type_printer : IRShow.default_expr_type_printer
    else
        Returns(nothing)
    end
    __postprinter = if isa(src, CodeInfo) && !isnothing(pc2effects)
        function (io::IO; idx::Int, @nospecialize(kws...))
            ___postprinter(io; idx, kws...)
            local effects = get(pc2effects, idx, nothing)
            effects === nothing && return
            print(io, ' ', effects)
        end
    else
        ___postprinter
    end
    _postprinter = if isa(src, CodeInfo) && !isnothing(pc2excts)
        function (io::IO; idx::Int, @nospecialize(kws...))
            __postprinter(io; idx, kws...)
            local exct = get(pc2excts, idx, nothing)
            exct === nothing && return
            print(io, ' ', ExctWrapper(exct))
        end
    else
        __postprinter
    end
    postprinter = if isa(src, CodeInfo) && !isnothing(pc2remarks)
        sort!(pc2remarks)
        unique!(pc2remarks) # abstract interpretation may have visited a same statement multiple times
        function (io::IO; idx::Int, @nospecialize(kws...))
            _postprinter(io; idx, kws...)
            for i = searchsorted(pc2remarks, idx=>"", by=((idx,msg),)->idx)
                printstyled(io, ' ', pc2remarks[i].second; color=:light_black, maxtypedepth)
            end
        end
    else
        _postprinter
    end

    should_print_stmt = hide_type_stable ? is_type_unstable : Returns(true)
    bb_color = (src isa IRCode && debuginfo === :compact) ? :normal : :light_black

    irshow_config = IRShow.IRShowConfig(preprinter, postprinter; should_print_stmt, bb_color)

    if !inline_cost && iswarn
        print(lambda_io, "Body")
        InteractiveUtils.warntype_type_printer(lambda_io; type=rettype, used=true)
        if get(lambda_io, :with_effects, false)::Bool
            print(lambda_io, ' ', effects)
        end
        println(lambda_io)
    else
        isa(mi, MethodInstance) || throw("`mi::MethodInstance` is required")
        cfg = src isa IRCode ? src.cfg : CC.compute_basic_blocks(src.code)
        max_bb_idx_size = length(string(length(cfg.blocks)))
        str = irshow_config.line_info_preprinter(lambda_io, " "^(max_bb_idx_size + 2), -1)
        callsite = Callsite(0, MICallInfo(mi, rettype, effects, exct), :invoke)
        println(lambda_io, "∘ ", "─"^(max_bb_idx_size), str, " ", callsite)
    end

    IRShow.show_ir(lambda_io, src, irshow_config)
    return nothing
end

function descend_into_callsite!(io::IO, tsn::TypedSyntaxNode;
    iswarn::Bool, hide_type_stable::Bool, type_annotations::Bool)
    sig, body = children(tsn)
    # We empty the body when filling kwargs
    istruncated = isempty(children(body))
    idxend = istruncated ? JuliaSyntax.last_byte(sig) : lastindex(tsn.source)
    maxtypedepth = CONFIG.type_depth_limit
    if !istruncated # If method only fills in default arguments
        printstyled(io, tsn; type_annotations, iswarn, hide_type_stable, idxend, maxtypedepth)
    end
end

function add_callsites!(d::AbstractDict, visited_mis::AbstractSet, diagnostics::AbstractVector,
    mi::MethodInstance, source_mi::MethodInstance=mi;
    optimize::Bool=true, annotate_source::Bool=false,
    interp::AbstractInterpreter=CthulhuInterpreter())

    callsites, src, rt = try
        (; src, rt, infos, slottypes, effects, codeinf) = lookup(interp, mi, optimize & !annotate_source)

        src = preprocess_ci!(src, mi, optimize & !annotate_source, CONFIG)
        if (optimize & !annotate_source) || isa(src, IRCode) # optimization might have deleted some statements
            infos = src.stmts.info
        else
            @assert length(src.code) == length(infos)
        end

        # We pass false as it doesn't affect callsites and skips fetching the method definition
        # using CodeTracking which is slow
        callsites, _ = find_callsites(interp, src, infos, mi, slottypes, optimize & !annotate_source, false)
        callsites, src, rt
    catch
        return nothing
    end

    for callsite in callsites
        callsite_mi = callsite.info isa MultiCallInfo ? nothing : get_mi(callsite)

        if !isnothing(callsite_mi) && callsite_mi ∉ visited_mis
            push!(visited_mis, callsite_mi)
            add_callsites!(d, visited_mis, diagnostics, callsite_mi, source_mi; optimize, annotate_source, interp)
        end
    end

    # Check if callsite is not just filling in default arguments and defined in same file as source_mi
    if mi == source_mi || mi.def.file != source_mi.def.file
        return nothing
    end
    tsn, _ = get_typed_sourcetext(mi, src, rt; warn=false)
    isnothing(tsn) && return nothing
    sig, body = children(tsn)
    # We empty the body when filling kwargs
    istruncated = isempty(children(body))
    istruncated && return nothing
    # We add new callsites unless we would have multiple callsites for the same source definition,
    # e.g. if f(x) = x is called with different types we print nothing.
    key = (mi.def.file, mi.def.line)
    if haskey(d, key)
        if !isnothing(d[key]) && mi != d[key].mi
            d[key] = nothing
            push!(diagnostics,
                TypedSyntax.Diagnostic(
                    isnothing(functionloc(mi)[1]) ? string(mi.file) : functionloc(mi)[1], mi.def.line,
                    TypedSyntax.DiagnosticKinds.Information,
                    "Cthulhu disabled: This function was called multiple times with different argument types"
                )
            )
        end
    else
        d[key] = (mi=mi, tsn=tsn)
    end
end

function show_variables(io, src, slotnames)
    println(io, "Variables")
    slottypes = src.slottypes
    for i = 1:length(slotnames)
        print(io, "  ", slotnames[i])
        if isa(slottypes, Vector{Any})
            InteractiveUtils.warntype_type_printer(io; type=slottypes[i], used=true)
        end
        println(io)
    end
    println(io)
end

# These are standard code views that don't need any special handling,
# This namedtuple maps toggle::Symbol to function
const CODEVIEWS = (;
    # typed=cthulhu_typed,
    llvm=cthulhu_llvm,
    native=cthulhu_native,
    ast=cthulhu_ast,
)

"""
    Cthulhu.Bookmark

A `Cthulhu.Bookmark` remembers a method marked by `b` key during a descent.
It can be used with the following functions:

* `descend(::Bookmark)`, `descend_code_typed(::Bookmark)`,
  `descend_code_warntype(::Bookmark)`: continue the descent.
* `code_typed(::Bookmark)`, `code_warntype([::IO,] ::Bookmark)`: show typed IR
* `code_llvm([::IO,] ::Bookmark)`: pretty-print LLVM IR
* `code_native([::IO,] ::Bookmark)`: pretty-print native code
"""
struct Bookmark
    mi::MethodInstance
    interp::AbstractInterpreter
end

"""
    Cthulhu.BOOKMARKS :: Vector{Bookmark}

During a descent, methods can be "bookmarked" by pressing `b` key.  It
pushes a [`Cthulhu.Bookmark`](@ref) into `Cthulhu.BOOKMARKS`.  This can be
used to, e.g., continue descending by `descend(Cthulhu.BOOKMARKS[end])`.
See [`Cthulhu.Bookmark`](@ref) for other usages.
"""
const BOOKMARKS = Bookmark[]

# Turn off `optimize` and `debuginfo` for default `show` so that the
# output is smaller.
function Base.show(
    io::IO, ::MIME"text/plain", b::Bookmark;
    optimize::Bool=false, debuginfo::AnyDebugInfo=:none, iswarn::Bool=false, hide_type_stable::Bool=false)
    world = get_inference_world(b.interp)
    CI, rt = InteractiveUtils.code_typed(b; optimize)
    (; interp, mi) = b
    (; effects) = lookup(interp, mi, optimize)
    if get(io, :typeinfo, Any) === Bookmark  # a hack to check if in Vector etc.
        print(io, Callsite(-1, MICallInfo(b.mi, rt, Effects()), :invoke))
        print(io, " (world: ", world, ")")
        return
    end
    println(io, "Cthulhu.Bookmark (world: ", world, ")")
    cthulhu_typed(io, debuginfo, CI, rt, nothing, effects, b.mi; iswarn, optimize, hide_type_stable, b.interp)
end

function InteractiveUtils.code_typed(b::Bookmark; optimize::Bool=true)
    (; interp, mi) = b
    (; src, rt, codeinf) = lookup(interp, mi, optimize)
    src = preprocess_ci!(src, mi, optimize, CONFIG)
    if src isa IRCode
        CC.replace_code_newstyle!(codeinf, src)
    end
    return codeinf => rt
end

InteractiveUtils.code_warntype(b::Bookmark; kw...) =
    InteractiveUtils.code_warntype(stdout::IO, b; kw...)
function InteractiveUtils.code_warntype(
    io::IO,
    b::Bookmark;
    optimize::Bool=false,
    debuginfo::AnyDebugInfo = :source,
    hide_type_stable::Bool = true,
    kw...,
)
    CI, rt = InteractiveUtils.code_typed(b; kw...)
    (; interp, mi) = b
    (; effects) = lookup(interp, mi, optimize)
    cthulhu_warntype(io, debuginfo, CI, rt, effects, b.mi; optimize, hide_type_stable, b.interp)
end

InteractiveUtils.code_llvm(b::Bookmark; kw...) = InteractiveUtils.code_llvm(stdout::IO, b; kw...)
InteractiveUtils.code_native(b::Bookmark; kw...) =
    InteractiveUtils.code_native(stdout::IO, b; kw...)

@static if VERSION < v"1.12.0-DEV.669"
    InteractiveUtils.code_llvm(
        io::IO,
        b::Bookmark;
        optimize = true,
        debuginfo = :source,
        dump_module = false,
        raw = false,
        config = CONFIG,
    ) = cthulhu_llvm(
        io,
        b.mi,
        optimize,
        debuginfo === :source,
        get_inference_world(b.interp),
        config,
        dump_module,
        raw,
    )

    InteractiveUtils.code_native(
        io::IO,
        b::Bookmark;
        optimize = true,
        debuginfo = :source,
        dump_module = false,
        raw = false,
        config = CONFIG,
    ) = cthulhu_native(
        io,
        b.mi,
        optimize,
        debuginfo === :source,
        get_inference_world(b.interp),
        config,
        dump_module,
        raw,
    )
else
    function InteractiveUtils.code_llvm(
        io::IO,
        b::Bookmark;
        optimize = true,
        debuginfo = :source,
        dump_module = false,
        raw = false,
        config = CONFIG,
    )
        src = CC.typeinf_code(b.interp, b.mi, true)
        return cthulhu_llvm(
            io,
            b.mi,
            src,
            optimize,
            debuginfo === :source,
            get_inference_world(b.interp),
            config,
            dump_module,
            raw,
        )
    end

    function InteractiveUtils.code_native(
        io::IO,
        b::Bookmark;
        optimize = true,
        debuginfo = :source,
        dump_module = false,
        raw = false,
        config = CONFIG,
    )
        src = CC.typeinf_code(b.interp, b.mi, true)
        return cthulhu_native(
            io,
            b.mi,
            src,
            optimize,
            debuginfo === :source,
            get_inference_world(b.interp),
            config,
            dump_module,
            raw,
        )
    end
end
