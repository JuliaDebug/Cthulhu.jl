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
        optimize, debuginfo ? :source : :none, Base.CodegenParams())
    highlight(io, dump, "llvm", config)
end

function cthulhu_native(io::IO, mi, optimize, debuginfo, params, config::CthulhuConfig)
    dump = InteractiveUtils._dump_function_linfo_native(
        mi, params.world,
        #=wrapper=# false, #=syntax=# config.asm_syntax,
        debuginfo ? :source : :none)
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
        @info "Could not retrieve AST. AST display requires Revise.jl to be loaded." meth
    end
end

function cthulhu_source(io::IO, mi, optimize, debuginfo, params, config::CthulhuConfig)
    meth = mi.def
    src, line = definition(String, meth)
    highlight(io, src, "julia", config)
end

cthulhu_warntype(args...) = cthulhu_warntype(stdout, args...)
function cthulhu_warntype(io::IO, src, rettype, debuginfo, stable_code)
    debuginfo = Base.IRShow.debuginfo(debuginfo)
    lineprinter = Base.IRShow.__debuginfo[debuginfo]
    lambda_io::IOContext = io
    if hasfield(typeof(src), :slotnames) && src.slotnames !== nothing
        slotnames = Base.sourceinfo_slotnames(src)
        lambda_io = IOContext(lambda_io, :SOURCE_SLOTNAMES => slotnames)
        show_variables(io, src, slotnames)
    end
    print(io, "Body")
    InteractiveUtils.warntype_type_printer(io, rettype, true)
    println(io)
    if isa(src, IRCode)
        show(io, src)
        # XXX this doesn't properly show warntype
    else
        print_stmt = stable_code ? (_, _, _) -> true : is_type_unstable
        show_ir(lambda_io, src, lineprinter(src), InteractiveUtils.warntype_type_printer;
                print_stmt)
    end
    return nothing
end


function cthulu_typed(io::IO, debuginfo_key, CI, rettype, mi, iswarn, stable_code)
    println(io)
    println(io, "│ ─ $(string(Callsite(-1, MICallInfo(mi, rettype), :invoke)))")

    if iswarn
        cthulhu_warntype(io, CI, rettype, debuginfo_key, stable_code)
    elseif isa(CI, IRCode)
        show(io, CI)
    else
        show(io, CI, debuginfo = debuginfo_key)
    end
    println(io)
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
const codeviews = (;
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
    params::CompilerParams
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
                   optimize = false, debuginfo = :none, iswarn=false)
    CI, rt = InteractiveUtils.code_typed(b, optimize = optimize)
    if get(io, :typeinfo, Any) === Bookmark  # a hack to check if in Vector etc.
        print(io, Callsite(-1, MICallInfo(b.mi, rt)), :invoke)
        print(io, " (world: ", b.params.world, ")")
        return
    end
    println(io, "Cthulhu.Bookmark (world: ", b.params.world, ")")
    cthulu_typed(io, debuginfo, CI, rt, b.mi, iswarn)
end

function InteractiveUtils.code_typed(b::Bookmark; optimize = true)
    (CI, rt, slottypes) = do_typeinf_slottypes(b.mi, optimize, b.params)
    preprocess_ci!(CI, b.mi, optimize, CONFIG)
    return CI => rt
end

InteractiveUtils.code_warntype(b::Bookmark; kw...) =
    InteractiveUtils.code_warntype(stdout, b; kw...)
function InteractiveUtils.code_warntype(io::IO, b::Bookmark; debuginfo = :source, kw...)
    CI, rt = InteractiveUtils.code_typed(b; kw...)
    cthulhu_warntype(io, CI, rt, debuginfo)
end

InteractiveUtils.code_llvm(b::Bookmark) = InteractiveUtils.code_llvm(stdout, b)
InteractiveUtils.code_llvm(io::IO, b::Bookmark; optimize = true, debuginfo = :source,
                           dump_module = false, config = CONFIG) =
    cthulhu_llvm(io, b.mi, optimize, debuginfo == :source, b.params, config, dump_module)

InteractiveUtils.code_native(b::Bookmark; kw...) =
    InteractiveUtils.code_native(stdout, b; kw...)
InteractiveUtils.code_native(io::IO, b::Bookmark; optimize = true, debuginfo = :source,
                             config = CONFIG) =
    cthulhu_native(io, b.mi, optimize, debuginfo == :source, b.params, config)

@nospecialize

using Base.IRShow: _stmt, _type, should_print_ssa_type, statementidx_lineinfo_printer,
    default_expr_type_printer, compute_basic_blocks, scan_ssa_use!, show_ir_stmt

function is_type_unstable(code::CodeInfo, idx::Int, used::BitSet)
    stmt = _stmt(code, idx)
    type = _type(code, idx)
    should_print_ssa_type(stmt) || return false
    return (idx in used) && type isa Type && (!Base.isdispatchelem(type) || type == Core.Box)
end

function show_ir(io::IO, code::CodeInfo, line_info_preprinter=statementidx_lineinfo_printer(code), line_info_postprinter=default_expr_type_printer; print_stmt = (_, _, _) -> true)
    stmts = code.code
    used = BitSet()
    cfg = compute_basic_blocks(stmts)
    for stmt in stmts
        scan_ssa_use!(push!, used, stmt)
    end
    bb_idx = 1

    for idx in 1:length(stmts)
        if print_stmt(code, idx, used)
            bb_idx = show_ir_stmt(io, code, idx, line_info_preprinter, line_info_postprinter, used, cfg, bb_idx)
        # this is the only functionality added vs base
        elseif bb_idx <= length(cfg.blocks) && idx == cfg.blocks[bb_idx].stmts.stop
            bb_idx += 1
        end
    end

    max_bb_idx_size = length(string(length(cfg.blocks)))
    line_info_preprinter(io, " "^(max_bb_idx_size + 2), 0)
    nothing
end

@specialize
