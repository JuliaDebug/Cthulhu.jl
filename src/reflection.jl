function cthulhu_llvm()

end

function cthulhu_native()

end


# TODO version check
cthulhu_warntype(args...) = cthulhu_warntype(stdout, args...)
function cthulhu_warntype(io::IO, src, rettype, debuginfo)
    debuginfo = Base.IRShow.debuginfo(debuginfo)
    lineprinter = Base.IRShow.__debuginfo[debuginfo]

    lambda_io::IOContext = stdout
    if src.slotnames !== nothing
        lambda_io = IOContext(lambda_io, :SOURCE_SLOTNAMES =>  Base.sourceinfo_slotnames(src))
    end
    print(io, "Body")
    InteractiveUtils.warntype_type_printer(io, rettype, true)
    println(io)
    Base.IRShow.show_ir(lambda_io, src, lineprinter(src), InteractiveUtils.warntype_type_printer)
    return nothing
end
