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

function cthulhu_llvm(io::IO, provider::AbstractProvider, state::CthulhuState, result::LookupResult; dump_module::Bool=false, raw::Bool=false)
    result.src === nothing && error("`result.src` must be a `CodeInfo` to use this function")
    (; config) = state
    (; optimize, debuginfo) = config
    world = get_inference_world(provider)
    dump = InteractiveUtils._dump_function_llvm(
        state.mi, result.src,
        #=wrapper=# false, !raw,
        dump_module, optimize, debuginfo !== :none ? :source : :none,
        Base.CodegenParams())
    highlight(io, dump, "llvm", config)
end

function cthulhu_native(io::IO, provider::AbstractProvider, state::CthulhuState, result::LookupResult; dump_module::Bool=false, raw::Bool=false)
    result.src === nothing && error("`result.src` must be a `CodeInfo` to use this function")
    (; config) = state
    (; debuginfo, asm_syntax) = config
    world = get_inference_world(provider)
    if dump_module
        dump = InteractiveUtils._dump_function_native_assembly(
            state.mi, result.src,
            #=wrapper=# false, #=syntax=# config.asm_syntax,
            debuginfo != debuginfo !== :none ? :source : :none,
            #=binary=# false, raw,
            Base.CodegenParams())
    else
        dump = InteractiveUtils._dump_function_native_disassembly(
            state.mi, world,
            #=wrapper=# false, #=syntax=# config.asm_syntax,
            debuginfo != debuginfo !== :none ? :source : :none,
            #=binary=# false)
    end
    highlight(io, dump, "asm", config)
end

function cthulhu_ast(io::IO, provider::AbstractProvider, state::CthulhuState, result::LookupResult)
    def = state.mi.def
    !isa(def, Method) && @warn "Can't show the AST because the definition is not a method."
    ast = definition(Expr, def)
    ast === nothing && return @warn "Could not retrieve AST of $def. AST display requires Revise.jl to be loaded."
    if !state.config.pretty_ast
        dump(io, ast; maxdepth=typemax(Int))
    else
        show(io, ast)
        # Meta.show_sexpr(io, ast)
        # Could even highlight the above as some kind-of LISP
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

function cthulhu_warntype(io::IO, provider::AbstractProvider, state::CthulhuState, result::LookupResult)
    @reset state.config.iswarn = true
    return cthulhu_typed(io, provider, state, result)
end

function cthulhu_source(io::IO, provider::AbstractProvider, state::CthulhuState, result::LookupResult)
    return cthulhu_typed(io, provider, state, result)
end

function cthulhu_typed(io::IO, provider::AbstractProvider, state::CthulhuState, result::LookupResult)
    (; mi, ci, config) = state
    src = something(result.ir, result.src)::Union{IRCode, CodeInfo}

    pc2remarks = !result.optimized & config.remarks ? get_pc_remarks(provider, ci) : nothing
    pc2effects = config.effects ? get_pc_effects(provider, ci) : nothing
    pc2excts = config.exception_types ? get_pc_excts(provider, ci) : nothing
    costs = result.optimized & config.inlining_costs ? get_inlining_costs(provider, mi, src) : nothing

    debuginfo = IRShow.debuginfo(config.debuginfo)
    lineprinter = __debuginfo[debuginfo]
    rettype = ignorelimited(result.rt)
    lambda_io = IOContext(io, :limit=>true)

    if isa(result.src, CodeInfo)
        tsn, _ = get_typed_sourcetext(mi, result.src, result.rt)
        if tsn !== nothing
            sig, body = children(tsn)
            # We empty the body when filling kwargs
            istruncated = isempty(children(body))
            idxend = istruncated ? JuliaSyntax.last_byte(sig) : lastindex(tsn.source)

            vscode_io = IOContext(
                config.view !== :source || config.jump_always && config.inlay_types_vscode ? devnull : lambda_io,
                :inlay_hints => config.inlay_types_vscode ? Dict{String,Vector{TypedSyntax.InlayHint}}() : nothing ,
                :diagnostics => config.diagnostics_vscode ? TypedSyntax.Diagnostic[] : nothing
            )

            source_io = ifelse(istruncated, lambda_io, vscode_io)
            printstyled(source_io, tsn; config.type_annotations, config.iswarn, config.hide_type_stable, idxend)
            println(source_io)

            callsite_diagnostics = TypedSyntax.Diagnostic[]
            if (config.diagnostics_vscode || config.inlay_types_vscode)
                vscode_io = IOContext(devnull, :inlay_hints => vscode_io[:inlay_hints], :diagnostics => vscode_io[:diagnostics])
                callsite_cis = Dict() # type annotation is a bit long so I skipped it, doesn't seem to affect performance
                visited_cis = Set{CodeInstance}((ci,))
                add_callsites!(callsite_cis, visited_cis, callsite_diagnostics, provider, ci, result)
                for callsite in values(callsite_cis)
                    if !isnothing(callsite)
                        descend_into_callsite!(vscode_io, callsite.tsn; config.type_annotations, config.iswarn, config.hide_type_stable)
                    end
                end
            end

            !isnothing(vscode_io[:diagnostics]) && append!(callsite_diagnostics, vscode_io[:diagnostics])
            TypedSyntax.display_diagnostics_vscode(callsite_diagnostics)
            TypedSyntax.display_inlay_hints_vscode(vscode_io)

            istruncated && @info "This method only fills in default arguments; descend into the body method to see the full source."
            config.view === :source && return
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

    # preprinter configuration
    preprinter = if costs !== nothing
        total_cost = sum(costs)
        nd = ndigits(total_cost)
        _lineprinter = lineprinter(src)
        function (io, linestart, idx)
            str = idx > 0   ? lpad(costs[idx], nd+1) :
                  idx == -1 ? lpad(total_cost, nd+1) :
                  " "^(nd+1)
            str = sprint(; context=:color=>true) do @nospecialize io
                printstyled(io, str; color=:green)
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
    ___postprinter = !config.type_annotations ? Returns(nothing) :
                     config.iswarn ? InteractiveUtils.warntype_type_printer :
                     IRShow.default_expr_type_printer
    __postprinter = pc2effects === nothing ? ___postprinter :
        function (io::IO; idx::Int, @nospecialize(kws...))
            ___postprinter(io; idx, kws...)
            local effects = get(pc2effects, idx, nothing)
            effects === nothing && return
            print(io, ' ', effects)
        end
    _postprinter = pc2excts === nothing ? __postprinter :
        function (io::IO; idx::Int, @nospecialize(kws...))
            __postprinter(io; idx, kws...)
            local exct = get(pc2excts, idx, nothing)
            exct === nothing && return
            print(io, ' ', ExctWrapper(exct))
        end
    postprinter = pc2remarks === nothing ? _postprinter : begin
        sort!(pc2remarks)
        unique!(pc2remarks) # abstract interpretation may have visited a same statement multiple times
        function (io::IO; idx::Int, @nospecialize(kws...))
            _postprinter(io; idx, kws...)
            for i = searchsorted(pc2remarks, idx=>"", by=((idx,msg),)->idx)
                printstyled(io, ' ', pc2remarks[i].second; color=:light_black)
            end
        end
    end

    should_print_stmt = config.hide_type_stable ? is_type_unstable : Returns(true)
    bb_color = (src isa IRCode && debuginfo === :compact) ? :normal : :light_black

    irshow_config = IRShow.IRShowConfig(preprinter, postprinter; should_print_stmt, bb_color)

    if !config.inlining_costs && config.iswarn
        print(lambda_io, "Body")
        InteractiveUtils.warntype_type_printer(lambda_io; type=rettype, used=true)
        if get(lambda_io, :effects, false)::Bool
            print(lambda_io, ' ', result.effects)
        end
        println(lambda_io)
    else
        cfg = src isa IRCode ? src.cfg : CC.compute_basic_blocks(src.code)
        max_bb_idx_size = length(string(length(cfg.blocks)))
        str = irshow_config.line_info_preprinter(lambda_io, " "^(max_bb_idx_size + 2), -1)
        callsite = Callsite(0, EdgeCallInfo(ci, rettype, result.effects, result.exct), :invoke)
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
    if !istruncated # If method only fills in default arguments
        printstyled(io, tsn; type_annotations, iswarn, hide_type_stable, idxend)
    end
end

function add_callsites!(d::AbstractDict, visited_cis::AbstractSet, diagnostics::AbstractVector,
                        provider::AbstractProvider, ci::CodeInstance, result::LookupResult,
                        source_ci::CodeInstance=ci)
    mi = get_mi(ci)
    (; ir, src, rt, infos, slottypes, effects, optimized) = result
    callsites, _ = find_callsites(provider, result, ci)

    for callsite in callsites
        info = callsite.info
        isa(info, MultiCallInfo) && continue
        isa(info, ConstPropCallInfo) && continue
        isa(info, SemiConcreteCallInfo) && continue

        callsite_ci = get_ci(callsite)
        isnothing(callsite_ci) && continue
        in(callsite_ci, visited_cis) && continue

        push!(visited_cis, callsite_ci)
        result = LookupResult(provider, callsite_ci, optimized)
        add_callsites!(d, visited_cis, diagnostics, provider, callsite_ci, result, source_ci)
    end

    # Check if callsite is not just filling in default arguments and defined in same file as source_ci
    if ci == source_ci || ci.def.def.file != source_ci.def.def.file
        return nothing
    end
    tsn, _ = get_typed_sourcetext(mi, something(src, ir)::Union{CodeInfo, IRCode}, rt; warn=false)
    isnothing(tsn) && return nothing
    sig, body = children(tsn)
    # We empty the body when filling kwargs
    istruncated = isempty(children(body))
    istruncated && return nothing
    # We add new callsites unless we would have multiple callsites for the same source definition,
    # e.g. if f(x) = x is called with different types we print nothing.
    key = (mi.def.file, mi.def.line)
    if haskey(d, key)
        if !isnothing(d[key]) && mi != d[key].ci.def
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
        d[key] = (ci=ci, tsn=tsn)
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
