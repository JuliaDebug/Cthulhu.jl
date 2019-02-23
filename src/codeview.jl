const asm_syntax = Ref(:att)

function cthulhu_llvm(io::IO, mi, optimize, debuginfo, params)
    dump = InteractiveUtils._dump_function_linfo(
        mi, params.world, #=native=# false,
        #=wrapper=# false, #=strip_ir_metadata=# true,
        #=dump_module=# false, #=syntax=# asm_syntax[],
        optimize, debuginfo ? :source : :none)
    print(io, dump)
end

function cthulhu_native(io::IO, mi, optimize, debuginfo, params)
    dump = InteractiveUtils._dump_function_linfo(
        mi, params.world, #=native=# true,
        #=wrapper=# false, #=strip_ir_metadata=# true,
        #=dump_module=# false, #=syntax=# asm_syntax[],
        optimize, debuginfo ? :source : :none)
    print(io, dump)
end

cthulhu_warntype(args...) = cthulhu_warntype(stdout, args...)
function cthulhu_warntype(io::IO, src, rettype, debuginfo)
    if VERSION < v"1.1.0-DEV.762"
    elseif VERSION < v"1.2.0-DEV.229"
        lineprinter = Base.IRShow.debuginfo[debuginfo]
    else
        debuginfo = Base.IRShow.debuginfo(debuginfo)
        lineprinter = Base.IRShow.__debuginfo[debuginfo]
    end

    lambda_io::IOContext = stdout
    if src.slotnames !== nothing
        lambda_io = IOContext(lambda_io, :SOURCE_SLOTNAMES =>  Base.sourceinfo_slotnames(src))
    end
    print(io, "Body")
    InteractiveUtils.warntype_type_printer(io, rettype, true)
    println(io)
    if VERSION < v"1.1.0-DEV.762"
        Base.IRShow.show_ir(lambda_io, src, InteractiveUtils.warntype_type_printer)
    else
        Base.IRShow.show_ir(lambda_io, src, lineprinter(src), InteractiveUtils.warntype_type_printer)
    end
    return nothing
end
