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
    @static if VERSION >= v"1.5.0-DEV.393"
        dump = InteractiveUtils._dump_function_linfo_llvm(
            mi, params.world,
            #=wrapper=# false, #=strip_ir_metadata=# true,
            dump_module,
            optimize, debuginfo ? :source : :none, Base.CodegenParams())
    else
        dump = InteractiveUtils._dump_function_linfo(
            mi, params.world, #=native=# false,
            #=wrapper=# false, #=strip_ir_metadata=# true,
            dump_module, #=syntax=# config.asm_syntax,
            optimize, debuginfo ? :source : :none)
    end
    highlight(io, dump, "llvm", config)
end

function cthulhu_native(io::IO, mi, optimize, debuginfo, params, config::CthulhuConfig)
    @static if VERSION >= v"1.5.0-DEV.393"
        dump = InteractiveUtils._dump_function_linfo_native(
            mi, params.world,
            #=wrapper=# false, #=syntax=# config.asm_syntax,
            debuginfo ? :source : :none)
    else
        dump = InteractiveUtils._dump_function_linfo(
            mi, params.world, #=native=# true,
            #=wrapper=# false, #=strip_ir_metadata=# true,
            #=dump_module=# false, #=syntax=# config.asm_syntax,
            optimize, debuginfo ? :source : :none)
    end
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
    if VERSION < v"1.1.0-DEV.762"
    elseif VERSION < v"1.2.0-DEV.229"
        lineprinter = Base.IRShow.debuginfo[debuginfo]
    else
        debuginfo = Base.IRShow.debuginfo(debuginfo)
        lineprinter = Base.IRShow.__debuginfo[debuginfo]
    end

    lambda_io::IOContext = io
    if src.slotnames !== nothing
        slotnames = Base.sourceinfo_slotnames(src)
        lambda_io = IOContext(lambda_io, :SOURCE_SLOTNAMES => slotnames)
        VERSION >= v"1.2" && show_variables(io, src, slotnames)
    end
    print(io, "Body")
    InteractiveUtils.warntype_type_printer(io, rettype, true)
    println(io)
    if VERSION < v"1.1.0-DEV.762"
        Base.IRShow.show_ir(lambda_io, src, InteractiveUtils.warntype_type_printer)
    else
        ir_printer = stable_code ? Base.IRShow.show_ir : show_ir
        ir_printer(lambda_io, src, lineprinter(src), InteractiveUtils.warntype_type_printer)
    end
    return nothing
end


function cthulu_typed(io::IO, debuginfo_key, CI, rettype, mi, iswarn, stable_code)
    println(io)
    println(io, "│ ─ $(string(Callsite(-1, MICallInfo(mi, rettype), :invoke)))")

    if iswarn
        cthulhu_warntype(io, CI, rettype, debuginfo_key, stable_code)
    elseif VERSION >= v"1.1.0-DEV.762"
        show(io, CI, debuginfo = debuginfo_key)
    else
        show(io, MIME"text/plain"(), CI=>rettype)
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
using Base.IRShow: compute_basic_blocks, scan_ssa_use!, should_print_ssa_type, print_stmt, GotoIfNot, GotoNode, PhiNode, block_for_inst
function show_ir(io::IO, code::Core.CodeInfo, line_info_preprinter, line_info_postprinter)
    newprinter = VERSION >= v"1.6.0-DEV.852"
    cols = displaysize(io)[2]
    used = BitSet()
    stmts = code.code
    types = code.ssavaluetypes
    cfg = compute_basic_blocks(stmts)
    max_bb_idx_size = length(string(length(cfg.blocks)))
    for stmt in stmts
        scan_ssa_use!(push!, used, stmt)
    end
    bb_idx = 1

    if isempty(used)
        maxlength_idx = 0
    else
        maxused = maximum(used)
        maxlength_idx = length(string(maxused))
    end
    for idx in eachindex(stmts)
        if !isassigned(stmts, idx)
            # This is invalid, but do something useful rather
            # than erroring, to make debugging easier
            printstyled(io, "#UNDEF\n", color=:red)
            continue
        end
        stmt = stmts[idx]
        show_type = types isa Vector{Any} && should_print_ssa_type(stmt)
        if types isa Vector{Any} # ignore types for pre-inference code
            if isassigned(types, idx) && show_type
                typ = types[idx]
                if (idx in used) && typ isa Type && (!Base.isdispatchelem(typ) || typ == Core.Box)
                else
                    continue
                end
            else
                continue
            end
        end
        # Compute BB guard rail
        if bb_idx > length(cfg.blocks)
            # If invariants are violated, print a special leader
            linestart = " "^(max_bb_idx_size + 2) # not inside a basic block bracket
            if newprinter
                inlining_indent = line_info_preprinter(io, linestart, idx)
            else
                inlining_indent = line_info_preprinter(io, linestart, code.codelocs[idx])
            end
            printstyled(io, "!!! ", "─"^max_bb_idx_size, color=:light_black)
        else
            bbrange = cfg.blocks[bb_idx].stmts
            bbrange = bbrange.start:bbrange.stop
            # Print line info update
            linestart = idx == first(bbrange) ? "  " : sprint(io -> printstyled(io, "│ ", color=:light_black), context=io)
            linestart *= " "^max_bb_idx_size
            if newprinter
                inlining_indent = line_info_preprinter(io, linestart, idx)
            else
                inlining_indent = line_info_preprinter(io, linestart, code.codelocs[idx])
            end
            if idx == first(bbrange)
                bb_idx_str = string(bb_idx)
                bb_pad = max_bb_idx_size - length(bb_idx_str)
                bb_type = length(cfg.blocks[bb_idx].preds) <= 1 ? "─" : "┄"
                printstyled(io, bb_idx_str, " ", bb_type, "─"^bb_pad, color=:light_black)
            elseif idx == last(bbrange) # print separator
                printstyled(io, "└", "─"^(1 + max_bb_idx_size), color=:light_black)
            else
                printstyled(io, "│ ", " "^max_bb_idx_size, color=:light_black)
            end
            if idx == last(bbrange)
                bb_idx += 1
            end
        end
        print(io, inlining_indent, " ")
        # convert statement index to labels, as expected by print_stmt
        if stmt isa Expr
            if stmt.head === :gotoifnot && length(stmt.args) == 2 && stmt.args[2] isa Int
                stmt = GotoIfNot(stmt.args[1], block_for_inst(cfg, stmt.args[2]::Int))
            elseif stmt.head === :enter && length(stmt.args) == 1 && stmt.args[1] isa Int
                stmt = Expr(:enter, block_for_inst(cfg, stmt.args[1]::Int))
            end
        elseif isa(stmt, GotoIfNot)
            stmt = GotoIfNot(stmt.cond, block_for_inst(cfg, stmt.dest))
        elseif stmt isa GotoNode
            stmt = GotoNode(block_for_inst(cfg, stmt.label))
        elseif stmt isa PhiNode
            e = stmt.edges
            if VERSION >= v"1.6.0-DEV.732"
                stmt = PhiNode(Int32[block_for_inst(cfg, Int(e[i])) for i in 1:length(e)], stmt.values)
            else
                stmt = PhiNode(Any[block_for_inst(cfg, Int(e[i])) for i in 1:length(e)], stmt.values)
            end
        end
        print_stmt(io, idx, stmt, used, maxlength_idx, true, show_type)
        if types isa Vector{Any} # ignore types for pre-inference code
            if !isassigned(types, idx)
                # This is an error, but can happen if passes don't update their type information
                printstyled(io, "::#UNDEF", color=:red)
            elseif show_type
                typ = types[idx]
                if typ isa Union && Base.is_expected_union(typ)
                    Base.emphasize(io, "::$typ", Base.warn_color()) # more mild user notification
                else
                    Base.emphasize(io, "::$typ")
                end
            end
        end
        println(io)
    end
    let linestart = " "^(max_bb_idx_size + 2)
        if newprinter
            line_info_preprinter(io, linestart, 0)
        else
            line_info_preprinter(io, linestart, typemin(Int32))
        end
    end
    nothing
end
@specialize
