descend_code_typed_impl(@nospecialize(args...); kwargs...) =
    descend_with_error_handling(args...; annotate_source=false, iswarn=false, kwargs...)

descend_code_warntype_impl(@nospecialize(args...); kwargs...) =
    descend_with_error_handling(args...; annotate_source=false, iswarn=true, optimize=false, kwargs...)

function descend_with_error_handling(args...; terminal=default_terminal(), kwargs...)
    @nospecialize
    try
        _descend(terminal, args...; kwargs...)
    catch x
        TypedSyntax.clear_all_vscode()
        if x isa InterruptException
            return nothing
        else
            rethrow(x)
        end
    end
    return nothing
end

function default_terminal()
    term_env = get(ENV, "TERM", @static Sys.iswindows() ? "" : "dumb")
    term = REPL.Terminals.TTYTerminal(term_env, stdin, stdout, stderr)
    return term
end

descend_impl(@nospecialize(args...); kwargs...) =
    descend_with_error_handling(args...; iswarn=true, kwargs...)

##
# _descend is the main driver function.
# src/reflection.jl has the tools to discover methods
# src/ui.jl provides the user facing interface to which _descend responds
##
function _descend(term::AbstractTerminal, provider::AbstractProvider, curs::AbstractCursor;
    override = nothing,
    debuginfo::Union{Symbol,DebugInfo}    = CONFIG.debuginfo,                     # default is compact debuginfo
    optimize::Bool                        = CONFIG.optimize,                      # default is true
    interruptexc::Bool                    = CONFIG.interruptexc,
    iswarn::Bool                          = CONFIG.iswarn,                        # default is false
    hide_type_stable::Union{Nothing,Bool} = CONFIG.hide_type_stable,
    verbose::Union{Nothing,Bool}          = nothing,
    remarks::Bool                         = CONFIG.remarks&!CONFIG.optimize,      # default is false
    with_effects::Bool                    = CONFIG.with_effects,                  # default is false
    exception_type::Bool                  = CONFIG.exception_type,                # default is false
    inline_cost::Bool                     = CONFIG.inline_cost&CONFIG.optimize,   # default is false
    type_annotations::Bool                = CONFIG.type_annotations,              # default is true
    annotate_source::Bool                 = CONFIG.annotate_source,               # default is true
    inlay_types_vscode::Bool              = CONFIG.inlay_types_vscode,            # default is true
    diagnostics_vscode::Bool              = CONFIG.diagnostics_vscode,            # default is true
    jump_always::Bool                     = CONFIG.jump_always,                   # default is false
    )

    if isnothing(hide_type_stable)
        hide_type_stable = something(verbose, false)
    end
    isnothing(verbose) || Base.depwarn("The `verbose` keyword argument to `Cthulhu.descend` is deprecated. Use `hide_type_stable` instead.")
    if isa(debuginfo, Symbol)
        debuginfo = getfield(DInfo, debuginfo)::DebugInfo
    end

    menu_options = (; cursor = '•', scroll_wrap = true)
    display_CI = true
    view_cmd = cthulhu_typed
    iostream = term.out_stream::IO
    function additional_descend(new_ci::CodeInstance)
        _descend(term, provider, new_ci;
                 debuginfo, optimize, interruptexc, iswarn, hide_type_stable, remarks,
                 with_effects, exception_type,
                 inline_cost, type_annotations, annotate_source,
                 inlay_types_vscode, diagnostics_vscode)
    end
    custom_toggles = (@__MODULE__).custom_toggles(provider)
    if !(custom_toggles isa Vector{CustomToggle})
        error(lazy"""
        invalid `$AbstractProvider` API:
        `$(Cthulhu.custom_toggles)(provider::$(typeof(provider))` is expected to return `Vector{CustomToggle}` object.
        """)
    end
    while true
        src = @something(override, get_ci(curs))
        do_optimize = optimize & !annotate_source
        result = lookup(provider, src, do_optimize)
        if result === nothing
            if should_regenerate_code_instance(provider, src)
                additional_descend(src)
                break
            end
        end
        # if is_constant(result)
        #     # This was inferred to a pure constant - we have no code to show,
        #     # but make something up that looks plausible.
        #     # TODO use `codeinfo_for_const`
        #     constant = extract_constant(result, src)
        #     callsites = Callsite[]
        #     if display_CI
        #         exct = cached_exception_type(codeinst)
        #         callsite = Callsite(-1, EdgeCallInfo(codeinst, codeinst.rettype, get_effects(codeinst), exct), :invoke)
        #         println(iostream)
        #         println(iostream, "│ ─ $callsite")
        #         println(iostream, "│  return ", Const(codeinst.rettype_const))
        #         println(iostream)
        #     end
        #     mi = get_mi(codeinst)
        #     @goto show_menu
        # end
        ci = get_ci(curs)
        mi = get_mi(ci)
        infkey = override isa InferenceResult ? override : ci
        pc2excts = exception_type ? get_pc_exct(provider, infkey) : nothing
        callsites, sourcenodes = find_callsites(provider, result, ci, annotate_source, pc2excts)

        if jump_always
            if isdefined(Main, :VSCodeServer) && Main.VSCodeServer isa Module && isdefined(Main.VSCodeServer, :openfile)
                Main.VSCodeServer.openfile(whereis(mi.def::Method)...; preserve_focus=true)
            else
                edit(whereis(mi.def::Method)...)
            end
        end

        if display_CI
            pc2remarks = remarks ? get_pc_remarks(provider, infkey) : nothing
            pc2effects = with_effects ? get_pc_effects(provider, infkey) : nothing
            printstyled(IOContext(iostream, :limit=>true), mi.def, '\n'; bold=true)
            if debuginfo == DInfo.compact
                str = let debuginfo=debuginfo, src=result.src, codeinf=result.codeinf, rt=result.rt,
                          iswarn=iswarn, hide_type_stable=hide_type_stable,
                          pc2remarks=pc2remarks, pc2effects=pc2effects, inline_cost=inline_cost, type_annotations=type_annotations
                    ioctx = IOContext(iostream,
                        :color => true,
                        :displaysize => displaysize(iostream), # displaysize doesn't propagate otherwise
                        :SOURCE_SLOTNAMES => source_slotnames(result),
                        :with_effects => with_effects,
                        :exception_type => exception_type)
                    stringify(ioctx) do lambda_io
                        cthulhu_typed(lambda_io, provider, debuginfo, annotate_source ? codeinf : src, rt, result.exct, result.effects, ci;
                                      iswarn, optimize, hide_type_stable,
                                      pc2remarks, pc2effects, pc2excts,
                                      inline_cost, type_annotations, annotate_source, inlay_types_vscode, diagnostics_vscode,
                                      jump_always)
                    end
                end
                # eliminate trailing indentation (see first item in bullet list in PR #189)
                rmatch = findfirst(r"\u001B\[90m\u001B\[(\d+)G( *)\u001B\[1G\u001B\[39m\u001B\[90m( *)\u001B\[39m$", str)
                if rmatch !== nothing
                    str = str[begin:prevind(str, first(rmatch))]
                end
                print(iostream, str)
            else
                lambda_io = IOContext(iostream,
                    :SOURCE_SLOTNAMES => source_slotnames(result),
                    :with_effects => with_effects,
                    :exception_type => exception_type)
                cthulhu_typed(lambda_io, provider, debuginfo, result.src, result.rt, result.exct, result.effects, ci;
                              iswarn, optimize, hide_type_stable,
                              pc2remarks, pc2effects, pc2excts,
                              inline_cost, type_annotations, annotate_source, inlay_types_vscode, diagnostics_vscode,
                              jump_always)
            end
            view_cmd = cthulhu_typed
        else
            display_CI = true
        end

        @label show_menu

        shown_callsites = annotate_source ? sourcenodes : callsites
        menu = CthulhuMenu(shown_callsites, with_effects, exception_type,
                           optimize & !annotate_source,
                           iswarn&get(iostream, :color, false)::Bool,
                           hide_type_stable, custom_toggles; menu_options...)
        usg = usage(view_cmd, annotate_source, optimize, iswarn, hide_type_stable,
                    debuginfo, remarks, with_effects, exception_type, inline_cost,
                    type_annotations, CONFIG.enable_highlighter, inlay_types_vscode,
                    diagnostics_vscode, jump_always, custom_toggles)
        cid = request(term, usg, menu)
        toggle = menu.toggle

        if toggle === nothing
            if cid == length(callsites) + 1
                break
            end
            if cid == -1
                interruptexc ? throw(InterruptException()) : break
            end
            callsite = callsites[cid]
            sourcenode = !isempty(sourcenodes) ? sourcenodes[cid] : nothing

            info = callsite.info
            if info isa MultiCallInfo
                show_sub_callsites = sub_callsites = let callsite=callsite
                    map(ci->Callsite(callsite.id, ci, callsite.head), info.callinfos)
                end
                if isempty(sub_callsites)
                    Core.eval(Main, quote
                        provider = $provider
                        mi = $mi
                        info = $info
                    end)
                    @error "Expected multiple callsites, but found none. Please fill an issue with a reproducing example."
                    continue
                end
                if sourcenode !== nothing
                    show_sub_callsites = let callsite=callsite
                        map(info.callinfos) do ci
                            p = Base.unwrap_unionall(get_ci(ci).def.specTypes).parameters
                            if isa(sourcenode, TypedSyntax.MaybeTypedSyntaxNode) && length(p) == length(JuliaSyntax.children(sourcenode)) + 1
                                newnode = copy(sourcenode)
                                for (i, child) in enumerate(JuliaSyntax.children(newnode))
                                    child.typ = p[i+1]
                                end
                                newnode
                            else
                                Callsite(callsite.id, ci, callsite.head)
                            end
                        end
                    end
                end
                menu = CthulhuMenu(show_sub_callsites, with_effects, exception_type,
                                   optimize & !annotate_source, false, false, custom_toggles;
                                   sub_menu=true, menu_options...)
                cid = request(term, "", menu)
                if cid == length(sub_callsites) + 1
                    continue
                end
                if cid == -1
                    interruptexc ? throw(InterruptException()) : break
                end

                callsite = sub_callsites[cid]
                info = callsite.info
            end

            # forcibly enter and inspect the frame, although the native interpreter gave up
            if info isa TaskCallInfo
                @info """
                Inference didn't analyze this call because it is a dynamic call:
                Cthulhu nevertheless is trying to descend into it for further inspection.
                """
                additional_descend(get_ci(info)::CodeInstance)
                continue
            elseif info isa RTCallInfo
                @info """
                This is a runtime call. You cannot descend into it.
                """
                @goto show_menu
            end

            # recurse
            next_cursor = navigate(curs, callsite)::Union{AbstractCursor,Nothing}
            if next_cursor === nothing
                continue
            end

            _descend(term, provider, next_cursor;
                     override = get_override(provider, info), debuginfo,
                     optimize, interruptexc,
                     iswarn, hide_type_stable,
                     remarks, with_effects, exception_type, inline_cost,
                     type_annotations, annotate_source,
                     inlay_types_vscode, diagnostics_vscode,
                     jump_always)

        elseif toggle === :warn
            iswarn ⊻= true
        elseif toggle === :with_effects
            with_effects ⊻= true
        elseif toggle === :exception_type
            exception_type ⊻= true
        elseif toggle === :hide_type_stable
            hide_type_stable ⊻= true
        elseif toggle === :inlay_types_vscode
            inlay_types_vscode ⊻= true
            TypedSyntax.clear_inlay_hints_vscode()
        elseif toggle === :diagnostics_vscode
            diagnostics_vscode ⊻= true
            TypedSyntax.clear_diagnostics_vscode()
        elseif toggle === :optimize
            optimize ⊻= true
            if remarks && optimize
                @warn "Disable optimization to see the inference remarks."
            end
        elseif toggle === :debuginfo
            debuginfo = DebugInfo((Int(debuginfo) + 1) % 3)
        elseif toggle === :remarks
            remarks ⊻= true
            if remarks && optimize
                @warn "Disable optimization to see the inference remarks."
            end
        elseif toggle === :inline_cost
            inline_cost ⊻= true
            if inline_cost && !optimize
                @warn "Enable optimization to see the inlining costs."
            end
        elseif toggle === :type_annotations
            type_annotations ⊻= true
        elseif toggle === :highlighter
            CONFIG.enable_highlighter ⊻= true
            if CONFIG.enable_highlighter
                @info "Using syntax highlighter $(CONFIG.highlighter)."
            else
                @info "Turned off syntax highlighter for Julia, LLVM and native code."
            end
            display_CI = false
        elseif toggle === :dump_params
            show_parameters(stdout, provider) # XXX: show to terminal instead?
            display_CI = false
        elseif toggle === :bookmark
            push!(BOOKMARKS, Bookmark(mi, interp))
            @info "The method is pushed at the end of `Cthulhu.BOOKMARKS`."
            display_CI = false
        elseif toggle === :revise
            # Call Revise.revise() without introducing a dependency on Revise
            id = Base.PkgId(UUID("295af30f-e4ad-537b-8983-00126c2a3abe"), "Revise")
            mod = get(Base.loaded_modules, id, nothing)
            if mod !== nothing
                revise = getfield(mod, :revise)::Function
                revise()
                ci = generate_code_instance(interp, mi)
                curs = update_cursor(curs, ci)
            else
                @warn "Failed to load Revise."
            end
        elseif toggle === :edit
            edit(whereis(mi.def::Method)...)
            display_CI = false
        elseif toggle === :jump_always
            jump_always ⊻= true
        elseif toggle === :typed
            view_cmd = cthulhu_typed
            annotate_source = false
            display_CI = true
        elseif toggle === :source
            view_cmd = cthulhu_typed
            annotate_source = true
            display_CI = true
        elseif toggle === :ast || toggle === :llvm || toggle === :native
            view_cmd = CODEVIEWS[toggle]
            world = get_inference_world(provider)
            ci = generate_code_instance(provider, mi)
            result = lookup(provider, ci, #=optimize=#true)
            println(iostream)
            view_cmd(iostream, mi, result.codeinf, result.optimized, debuginfo, world, CONFIG)
            display_CI = false
        else
            local i = findfirst(ct->ct.toggle === toggle, custom_toggles)
            @assert i !== nothing
            ct = custom_toggles[i]
            onoff = ct.onoff ⊻= true
            if onoff
                curs = ct.callback_on(curs)
            else
                curs = ct.callback_off(curs)
            end
            if !(curs isa AbstractCursor)
                local f = onoff ? "callback_on" : "callback_off"
                error(lazy"""
                invalid `$AbstractInterpreter` API:
                `f` callback is expected to return `AbstractCursor` object.
                """)
            end
        end
        println(iostream)
    end

    TypedSyntax.clear_all_vscode()
end

function is_constant(result::LookupResult)
    return result.src === nothing
end

function extract_constant(result::LookupResult, ci::CodeInstance)
    return result.src === nothing
end

function source_slotnames(result::LookupResult)
    result.codeinf === nothing && return false
    return Base.sourceinfo_slotnames(result.codeinf)
end
