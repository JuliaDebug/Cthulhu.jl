abstract type AbstractCursor; end
struct CthulhuCursor <: AbstractCursor
    mi::MethodInstance
end

function update_mi(curs::CthulhuCursor, mi::MethodInstance)
    CthulhuCursor(mi)
end

# This method is optional, but should be implemented if there is
# a sensible default cursor for a MethodInstance
AbstractCursor(interp::CthulhuInterpreter, mi::MethodInstance) =
    CthulhuCursor(mi)

function get_optimized_code(interp::CthulhuInterpreter,
                                curs::CthulhuCursor)
    return interp.opt[curs.mi].inferred
end

get_mi(curs::CthulhuCursor) = curs.mi

function get_remarks(interp::CthulhuInterpreter, curs::CthulhuCursor)
    get(interp.remarks, mi, nothing)
end

function get_effects(interp::CthulhuInterpreter, mi::MethodInstance, opt::Bool)
    get_effects(opt ? interp.opt : interp.unopt, mi)
end

function navigate(curs::AbstractCursor, callsite)
    typeof(curs)(get_mi(callsite))
end

function can_descend(interp::CthulhuInterpreter, key, optimize::Bool)
    haskey(optimize ? interp.opt : interp.unopt, key)
end
