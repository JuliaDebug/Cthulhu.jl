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

function cthulhu_llvm(io::IO, mi, optimize, debuginfo, interp::CthulhuInterpreter,
                      config::CthulhuConfig, dump_module::Bool=false, raw::Bool=false)
    dump = @static if VERSION ≥ v"1.10.0-DEV.1386"
        InteractiveUtils._dump_function_llvm(
            mi, get_world_counter(interp),
            #=wrapper=# false, !raw,
            dump_module, optimize, debuginfo != DInfo.none ? :source : :none,
            Base.CodegenParams())
    else
        InteractiveUtils._dump_function_linfo_llvm(
            mi, get_world_counter(interp),
            #=wrapper=# false, #=strip_ir_metadata=# true,
            dump_module, optimize, debuginfo != DInfo.none ? :source : :none,
            Base.CodegenParams())
    end
    highlight(io, dump, "llvm", config)
end

function cthulhu_native(io::IO, mi, optimize, debuginfo, interp::CthulhuInterpreter,
                        config::CthulhuConfig, dump_module::Bool=false, raw::Bool=false)
    dump = @static if VERSION ≥ v"1.10.0-DEV.1386"
        if dump_module
            InteractiveUtils._dump_function_native_assembly(
                mi, get_world_counter(interp),
                #=wrapper=# false, #=syntax=# config.asm_syntax,
                debuginfo != DInfo.none ? :source : :none,
                #=binary=# false, raw,
                Base.CodegenParams())
        else
            InteractiveUtils._dump_function_native_disassembly(
                mi, get_world_counter(interp),
                #=wrapper=# false, #=syntax=# config.asm_syntax,
                debuginfo != DInfo.none ? :source : :none,
                #=binary=# false)
        end
    else
        InteractiveUtils._dump_function_linfo_native(
            mi, get_world_counter(interp),
            #=wrapper=# false, #=syntax=# config.asm_syntax,
            debuginfo != DInfo.none ? :source : :none,
            #=binary=# false)
    end
    highlight(io, dump, "asm", config)
end

function cthulhu_ast(io::IO, mi, optimize, debuginfo, ::CthulhuInterpreter, config::CthulhuConfig)
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

using Base.IRShow: IRShow, _stmt, _type, should_print_ssa_type, IRShowConfig, show_ir

const __debuginfo = merge(IRShow.__debuginfo, Dict(
    :compact => src -> src isa CodeInfo ? __debuginfo[:source](src)
                                        : IRShow.inline_linfo_printer(src)
))

function is_type_unstable(code::Union{IRCode, CodeInfo}, idx::Int, used::BitSet)
    stmt = _stmt(code, idx)
    type = _type(code, idx)
    should_print_ssa_type(stmt) || return false
    # `used` only contains used SSA values and ignores slots
    in_use = in(idx, used) || Meta.isexpr(stmt, :(=))
    return in_use && is_type_unstable(type)
end
is_type_unstable(@nospecialize(type)) = type isa Type && (!Base.isdispatchelem(type) || type == Core.Box)

cthulhu_warntype(args...; kwargs...) = cthulhu_warntype(stdout::IO, args...; kwargs...)
function cthulhu_warntype(io::IO, debuginfo::AnyDebugInfo,
    src::Union{CodeInfo,IRCode}, @nospecialize(rt), effects::Effects, mi::Union{Nothing,MethodInstance}=nothing;
    hide_type_stable::Bool=false, inline_cost::Bool=false, interp::CthulhuInterpreter=CthulhuInterpreter())
    if inline_cost
        isa(mi, MethodInstance) || error("Need a MethodInstance to show inlining costs. Call `cthulhu_typed` directly instead.")
    end
    cthulhu_typed(io, debuginfo, src, rt, effects, mi; iswarn=true, hide_type_stable, inline_cost, interp)
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
    src::Union{CodeInfo,IRCode}, @nospecialize(rt), effects::Effects, mi::Union{Nothing,MethodInstance};
    iswarn::Bool=false, hide_type_stable::Bool=false,
    pc2remarks::Union{Nothing,PC2Remarks}=nothing, pc2effects::Union{Nothing,PC2Effects}=nothing,
    inline_cost::Bool=false, type_annotations::Bool=true, annotate_source::Bool=false,
    interp::AbstractInterpreter=CthulhuInterpreter())

    debuginfo = IRShow.debuginfo(debuginfo)
    lineprinter = __debuginfo[debuginfo]
    rettype = ignorelimited(rt)
    lambda_io = IOContext(io, :limit=>true)

    if annotate_source && isa(src, CodeInfo)
        tsn, _ = get_typed_sourcetext(mi, src, rt)
        if tsn !== nothing
            sig, body = children(tsn)
            # We empty the body when filling kwargs
            istruncated = isempty(children(body))
            idxend = istruncated ? JuliaSyntax.last_byte(sig) : lastindex(tsn.source)
            if any(iszero, src.codelocs)
                @warn "Some line information is missing, type-assignment may be incomplete"
            end
            if src.slottypes === nothing
                @warn "Inference terminated in an incomplete state due to argument-type changes during recursion"
            end
            printstyled(lambda_io, tsn; type_annotations, iswarn, hide_type_stable, idxend)
            println(lambda_io)
            istruncated && @info "This method only fills in default arguments; descend into the body method to see the full source."
            return nothing
        end
    end

    if isa(src, CodeInfo)
        # we're working on pre-optimization state, need to ignore `LimitedAccuracy`
        src = copy(src)
        src.ssavaluetypes = mapany(ignorelimited, src.ssavaluetypes::Vector{Any})
        src.rettype = ignorelimited(src.rettype)

        if src.slotnames !== nothing
            slotnames = Base.sourceinfo_slotnames(src)
            lambda_io = IOContext(lambda_io, :SOURCE_SLOTNAMES => slotnames)
            show_variables(io, src, slotnames)
        end
    end

    # preprinter configuration
    preprinter = if src isa IRCode && inline_cost
        isa(mi, MethodInstance) || throw("`mi::MethodInstance` is required")
        if isa(src, IRCode)
            if VERSION < v"1.11.0-DEV.258"
                code = src.stmts.inst
            else
                code = src.stmts.stmt
            end
        else
            code = src.code
        end
        cst = Vector{Int}(undef, length(code))
        params = CC.OptimizationParams(interp)
        @static if VERSION ≥ v"1.11.0-DEV.32"
            CC.statement_costs!(cst, code, src, sptypes(mi.sparam_vals), params)
        else
            CC.statement_costs!(cst, code, src, sptypes(mi.sparam_vals), false, params)
        end
        total_cost = sum(cst)
        nd = ndigits(total_cost)
        _lineprinter = lineprinter(src)
        function (io, linestart, idx)
            str = idx > 0   ? lpad(cst[idx], nd+1) :
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
    __postprinter = if type_annotations
        iswarn ? InteractiveUtils.warntype_type_printer : IRShow.default_expr_type_printer
    else
        Returns(nothing)
    end
    _postprinter = if isa(src, CodeInfo) && !isnothing(pc2effects)
        @static if VERSION ≥ v"1.9-"
            function (io::IO; idx::Int, @nospecialize(kws...))
                __postprinter(io; idx, kws...)
                local effects = get(pc2effects, idx, nothing)
                effects === nothing && return
                print(io, ' ', effects)
            end
        else
            function (io::IO, @nospecialize(typ), used::Bool)
                __postprinter(io, typ, used)
                haskey(io, :idx) || return
                idx = io[:idx]::Int
                local effects = get(pc2effects, idx, nothing)
                effects === nothing && return
                print(io, ' ', effects)
            end
        end
    else
        __postprinter
    end
    postprinter = if isa(src, CodeInfo) && !isnothing(pc2remarks)
        sort!(pc2remarks)
        unique!(pc2remarks) # abstract interpretation may have visited a same statement multiple times
        @static if VERSION ≥ v"1.9-"
            function (io::IO; idx::Int, @nospecialize(kws...))
                _postprinter(io; idx, kws...)
                for i = searchsorted(pc2remarks, idx=>"", by=((idx,msg),)->idx)
                    printstyled(io, ' ', pc2remarks[i].second; color=:light_black)
                end
            end
        else
            function (io::IO, @nospecialize(typ), used::Bool)
                _postprinter(io, typ, used)
                haskey(io, :idx) || return
                idx = io[:idx]::Int
                for i = searchsorted(pc2remarks, idx=>"", by=((idx,msg),)->idx)
                    printstyled(io, ' ', pc2remarks[i].second; color=:light_black)
                end
            end
        end
    else
        _postprinter
    end

    should_print_stmt = hide_type_stable ? is_type_unstable : Returns(true)
    bb_color = (src isa IRCode && debuginfo === :compact) ? :normal : :light_black

    irshow_config = IRShowConfig(preprinter, postprinter; should_print_stmt, bb_color)

    if iswarn
        print(lambda_io, "Body")
        @static if VERSION ≥ v"1.9-"
            # https://github.com/JuliaLang/julia/pull/46574
            InteractiveUtils.warntype_type_printer(lambda_io; type=rettype, used=true)
        else
            InteractiveUtils.warntype_type_printer(lambda_io, rettype, true)
        end
        if get(lambda_io, :with_effects, false)::Bool
            print(lambda_io, ' ', effects)
        end
        println(lambda_io)
    else
        isa(mi, MethodInstance) || throw("`mi::MethodInstance` is required")
        cfg = src isa IRCode ? src.cfg : Core.Compiler.compute_basic_blocks(src.code)
        max_bb_idx_size = length(string(length(cfg.blocks)))
        str = irshow_config.line_info_preprinter(lambda_io, " "^(max_bb_idx_size + 2), -1)
        callsite = Callsite(0, MICallInfo(mi, rettype, effects), :invoke)
        println(lambda_io, "∘ ", "─"^(max_bb_idx_size), str, " ", callsite)
    end

    show_ir(lambda_io, src, irshow_config)
    return nothing
end

@static if VERSION >= v"1.10.0-DEV.552"
    using Core.Compiler: VarState
    sptypes(sparams) = VarState[VarState.(sparams, false)...]
else
    sptypes(sparams) = Any[sparams...]
end

function show_variables(io, src, slotnames)
    println(io, "Variables")
    slottypes = src.slottypes
    for i = 1:length(slotnames)
        print(io, "  ", slotnames[i])
        if isa(slottypes, Vector{Any})
            @static if VERSION ≥ v"1.9-"
                # https://github.com/JuliaLang/julia/pull/46574
                InteractiveUtils.warntype_type_printer(io; type=slottypes[i], used=true)
            else
                InteractiveUtils.warntype_type_printer(io, slottypes[i], true)
            end
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
    world = get_world_counter(b.interp)
    CI, rt = InteractiveUtils.code_typed(b; optimize)
    (; interp, mi) = b
    (; effects) = lookup(interp, mi, optimize)
    if get(io, :typeinfo, Any) === Bookmark  # a hack to check if in Vector etc.
        print(io, Callsite(-1, MICallInfo(b.mi, rt, Effects()), :invoke))
        print(io, " (world: ", world, ")")
        return
    end
    println(io, "Cthulhu.Bookmark (world: ", world, ")")
    cthulhu_typed(io, debuginfo, CI, rt, effects, b.mi; iswarn, hide_type_stable, b.interp)
end

function InteractiveUtils.code_typed(b::Bookmark; optimize::Bool=true)
    (; interp, mi) = b
    (; src, rt, codeinf) = lookup(interp, mi, optimize)
    src = preprocess_ci!(src, mi, optimize, CONFIG)
    if src isa IRCode
        @static if VERSION ≥ v"1.10.0-DEV.870"
            CC.replace_code_newstyle!(codeinf, src)
        else
            nargs = Int((mi.def::Method).nargs) - 1
            CC.replace_code_newstyle!(codeinf, src, nargs)
        end
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
    cthulhu_warntype(io, debuginfo, CI, rt, effects, b.mi; hide_type_stable, b.interp)
end

InteractiveUtils.code_llvm(b::Bookmark; kw...) = InteractiveUtils.code_llvm(stdout::IO, b; kw...)
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
    b.interp,
    config,
    dump_module,
    raw,
)

InteractiveUtils.code_native(b::Bookmark; kw...) =
    InteractiveUtils.code_native(stdout::IO, b; kw...)
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
    b.interp,
    config,
    dump_module,
    raw,
)
