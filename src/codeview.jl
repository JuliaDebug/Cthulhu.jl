function cthulhu_llvm()
    @info "Cthulhu can't display LLVM IR yet"
    return nothing
end

function cthulhu_native()
    @info "Cthulhu can't display assembly yet"
    return nothing
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
