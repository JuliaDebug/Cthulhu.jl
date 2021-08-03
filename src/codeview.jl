highlighter_exists(config::CthulhuConfig) =
    Sys.which(config.highlighter.exec[1]) !== nothing

__init__() = CONFIG.enable_highlighter = highlighter_exists(CONFIG)

function highlight(io, x, lexer, config::CthulhuConfig)
    _print = endswith(x, '\n') ? print : println
    config.enable_highlighter || return _print(io, x)
    if !highlighter_exists(config)
        @warn "Highlighter command $(config.highlighter.exec[1]) does not exist."
        return _print(io, x)
    end
    cmd = `$(config.highlighter) $lexer`
    open(pipeline(cmd; stdout=io, stderr=stderr), "w") do io
        _print(io, x)
    end
end

function cthulhu_llvm(io::IO, mi, optimize, debuginfo, params, config::CthulhuConfig,
                      dump_module = false)
    dump = InteractiveUtils._dump_function_linfo_llvm(
        mi, params.world,
        #=wrapper=# false, #=strip_ir_metadata=# true,
        dump_module,
        optimize, debuginfo != DInfo.none ? :source : :none,
        Base.CodegenParams())
    highlight(io, dump, "llvm", config)
end

function cthulhu_native(io::IO, mi, optimize, debuginfo, params, config::CthulhuConfig)
    dump = InteractiveUtils._dump_function_linfo_native(
        mi, params.world,
        #=wrapper=# false, #=syntax=# config.asm_syntax,
        debuginfo != DInfo.none ? :source : :none,
        #=binary=# false)
    highlight(io, dump, "asm", config)
end

function cthulhu_ast(io::IO, mi, optimize, debuginfo, params, config::CthulhuConfig)
    meth = mi.def
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

function cthulhu_source(io::IO, mi, optimize, debuginfo, params, config::CthulhuConfig)
    meth = mi.def
    def = definition(String, meth)
    if isnothing(def)
        return @warn "couldn't retrieve source of $meth"
    end
    src, line = def
    highlight(io, src, "julia", config)
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
    return (idx in used) && type isa Type && (!Base.isdispatchelem(type) || type == Core.Box)
end

cthulhu_warntype(args...; kwargs...) = cthulhu_warntype(stdout::IO, args...; kwargs...)
function cthulhu_warntype(io::IO, debuginfo::Union{DebugInfo,Symbol},
    src::Union{CodeInfo,IRCode}, @nospecialize(rt), mi::Union{Nothing,MethodInstance} = nothing;
    verbose::Bool=true, inline_cost::Bool=false)
    if inline_cost
        @assert isa(mi, MethodInstance) "`mi::MethodInstance` is required"
    end
    cthulhu_typed(io, debuginfo, src, rt, mi; iswarn=true, verbose, inline_cost)
    return nothing
end

cthulhu_typed(io::IO, debuginfo::DebugInfo, args...; kwargs...) =
    cthulhu_typed(io, Symbol(debuginfo), args...; kwargs...)
function cthulhu_typed(io::IO, debuginfo::Symbol,
    src::Union{CodeInfo,IRCode}, @nospecialize(rt), mi::Union{Nothing,MethodInstance};
    iswarn::Bool=false, verbose::Bool=true, inline_cost::Bool=false)
    debuginfo = IRShow.debuginfo(debuginfo)
    lineprinter = __debuginfo[debuginfo]
    rettype = ignorelimited(rt)
    lambda_io::IOContext = io

    if isa(src, Core.CodeInfo)
        # we're working on pre-optimization state, need to ignore `LimitedAccuracy`
        src = copy(src)
        src.ssavaluetypes = Base.mapany(ignorelimited, src.ssavaluetypes::Vector{Any})
        src.rettype = ignorelimited(src.rettype)

        if src.slotnames !== nothing
            slotnames = Base.sourceinfo_slotnames(src)
            lambda_io = IOContext(lambda_io, :SOURCE_SLOTNAMES => slotnames)
            iswarn && show_variables(io, src, slotnames)
        end
    end

    if iswarn
        print(io, "Body")
        InteractiveUtils.warntype_type_printer(io, rettype, true)
        println(io)
    else
        @assert isa(mi, MethodInstance) "`mi::MethodInstance` is required"
        println(io, "│ ─ $(string(Callsite(-1, MICallInfo(mi, rettype), :invoke)))")
    end

    if src isa IRCode && inline_cost
        @assert isa(mi, MethodInstance) "`mi::MethodInstance` is required"
        code = src isa IRCode ? src.stmts.inst : src.code
        cst = Vector{Int}(undef, length(code))
        params = Core.Compiler.OptimizationParams(Core.Compiler.NativeInterpreter())
        maxcost = Core.Compiler.statement_costs!(cst, code, src, Any[mi.sparam_vals...], false, params)
        nd = ndigits(maxcost)
        _lineprinter = lineprinter(src)
        function preprinter(io, linestart, idx)
            str = idx > 0 ? lpad(cst[idx], nd+1) : " "^(nd+1)
            str = sprint(io -> Base.printstyled(io, str; color=:green); context=:color=>true)
            return str * _lineprinter(io, linestart, idx)
        end
    else
        preprinter = lineprinter(src)
    end
    postprinter = iswarn ? InteractiveUtils.warntype_type_printer : IRShow.default_expr_type_printer

    should_print_stmt = (iswarn || src isa IRCode || verbose) ? Returns(true) : is_type_unstable
    bb_color = (src isa IRCode && debuginfo === :compact) ? :normal : :light_black

    irshow_config = IRShowConfig(preprinter, postprinter; should_print_stmt, bb_color)

    show_ir(lambda_io, src, irshow_config)
    return nothing
end

function show_variables(io, src, slotnames)
    println(io, "Variables")
    slottypes = src.slottypes
    for i = 1:length(slotnames)
        print(io, "  ", slotnames[i])
        if isa(slottypes, Vector{Any})
            InteractiveUtils.warntype_type_printer(io, slottypes[i], true)
        end
        println(io)
    end
    println(io)
end

# These are standard code views that don't need any special handling,
# This namedtuple maps toggle::Symbol to function
const CODEVIEWS = (;
    llvm=cthulhu_llvm,
    native=cthulhu_native,
    ast=cthulhu_ast,
    source=cthulhu_source,
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
    interp::CthulhuInterpreter
end

"""
    Cthulhu.BOOKMARKS :: Vector{Bookmark}

During a descent, methods can be "bookmarked" by pressing `b` key.  It
pushes a [`Cthulhu.Bookmark`](@ref) into `Cthulhu.BOOKMARKS`.  This can be
used to, e.g., continue descending by `descend(Cthulhu.BOOKMARKS[end])`.
See [`Cthulhu.Bookmark`](@ref) for other usages.
"""
const BOOKMARKS = Bookmark[]

# Default `show` is broken for `Core.Compiler.Params`. Trying not invoke it.
Base.show(io::IO, b::Bookmark) =
    print(io, "Cthulhu.Bookmark(", b.mi, ", ::", CompilerParams, ")")

# Turn off `optimize` and `debuginfo` for default `show` so that the
# output is smaller.
function Base.show(io::IO, ::MIME"text/plain", b::Bookmark;
                   optimize = false, debuginfo = :none, iswarn=false, verbose=true)
    world = b.interp.native.world
    CI, rt = InteractiveUtils.code_typed(b, optimize = optimize)
    if get(io, :typeinfo, Any) === Bookmark  # a hack to check if in Vector etc.
        print(io, Callsite(-1, MICallInfo(b.mi, rt), :invoke))
        print(io, " (world: ", world, ")")
        return
    end
    println(io, "Cthulhu.Bookmark (world: ", world, ")")
    cthulhu_typed(io, debuginfo, CI, rt, b.mi; iswarn, verbose)
end

function InteractiveUtils.code_typed(b::Bookmark; optimize = true)
    interp = b.interp
    mi = b.mi
    (ci, rt) = lookup(interp, mi, optimize)
    ci = preprocess_ci!(ci, mi, optimize, CONFIG)
    if ci isa IRCode
        ir = ci
        ci = copy(interp.unopt[mi].src)
        nargs = Int(mi.def.nargs) - 1
        Core.Compiler.replace_code_newstyle!(ci, ir, nargs)
    end
    return ci => rt
end

InteractiveUtils.code_warntype(b::Bookmark; kw...) =
    InteractiveUtils.code_warntype(stdout::IO, b; kw...)
function InteractiveUtils.code_warntype(
    io::IO,
    b::Bookmark;
    debuginfo = :source,
    verbose = true,
    kw...,
)
    CI, rt = InteractiveUtils.code_typed(b; kw...)
    cthulhu_warntype(io, debuginfo, CI, rt, b.mi; verbose)
end

InteractiveUtils.code_llvm(b::Bookmark) = InteractiveUtils.code_llvm(stdout::IO, b)
InteractiveUtils.code_llvm(
    io::IO,
    b::Bookmark;
    optimize = true,
    debuginfo = :source,
    dump_module = false,
    config = CONFIG,
) = cthulhu_llvm(
    io,
    b.mi,
    optimize,
    debuginfo == :source,
    b.interp.native,
    config,
    dump_module,
)

InteractiveUtils.code_native(b::Bookmark; kw...) =
    InteractiveUtils.code_native(stdout::IO, b; kw...)
InteractiveUtils.code_native(
    io::IO,
    b::Bookmark;
    optimize = true,
    debuginfo = :source,
    config = CONFIG,
) = cthulhu_native(io, b.mi, optimize, debuginfo == :source, b.interp.native, config)
