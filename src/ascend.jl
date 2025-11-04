function ascend_impl(
        term, mi;
        interp=Base.Compiler.NativeInterpreter(),
        provider::AbstractProvider=AbstractProvider(interp),
        pagesize::Int=10, dynamic::Bool=false, maxsize::Int=pagesize,
        menu_options=(; pagesize), kwargs...)
    root = Cthulhu.treelist(mi)
    root === nothing && return
    menu = TreeMenu(root; pagesize, dynamic, maxsize)
    choice = menu.current
    while choice !== nothing
        menu.chosen = false
        choice = TerminalMenus.request(term, "Choose a call for analysis (q to quit):", menu; cursor=menu.currentidx)
        browsecodetyped = true
        if choice !== nothing
            node = menu.current
            mi = Cthulhu.instance(node.data.nd)
            if !isroot(node)
                # Help user find the sites calling the parent
                parent = Cthulhu.instance(node.parent.data.nd)
                ulocs = find_caller_of(provider, parent, mi; allow_unspecialized=true)
                if !isempty(ulocs)
                    ulocs = [(k[1], maybe_fix_path(String(k[2])), k[3]) => v for (k, v) in ulocs]
                    strlocs = [string(" "^k[3] * '"', k[2], "\", ", k[1], ": lines ", v) for (k, v) in ulocs]
                    explain_inlining = length(ulocs) > 1 ? "(including inlined callers represented by indentation) " : ""
                    push!(strlocs, "Browse typed code")
                    linemenu = TerminalMenus.RadioMenu(strlocs; charset=:ascii)
                    browsecodetyped = false
                    choice2 = 1
                    while choice2 != -1
                        promptstr = sprint(parent, explain_inlining; context=:color=>get(term, :color, false)) do iobuf, mip, exi
                            printstyled(iobuf, "\nOpen an editor at a possible caller of\n  "; color=:light_cyan)
                            print(iobuf, parent)
                            printstyled(iobuf, "\n$(explain_inlining)or browse typed code:"; color=:light_cyan)
                        end
                        choice2 = TerminalMenus.request(term, promptstr, linemenu; cursor=choice2)
                        if 0 < choice2 < length(strlocs)
                            loc, lines = ulocs[choice2]
                            edit(loc[2], first(lines))
                        elseif choice2 == length(strlocs)
                            browsecodetyped = true
                            break
                        end
                    end
                end
            end
            if !isa(mi, MethodInstance)
                error("You can only descend into known calls. If you tried to descend into a runtime-dispatched signature, try its caller instead.")
            end
            # The main application of `ascend` is finding cases of non-inferrability, so the
            # warn highlighting is useful.
            browsecodetyped && _descend(term, mi; provider, view=:source, iswarn=true, optimize=false, menu_options, kwargs...)
        end
    end
end
ascend_impl(mi; kwargs...) = ascend_impl(default_terminal(), mi; kwargs...)
