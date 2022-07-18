abstract type AbstractCursor; end
struct CthulhuCursor <: AbstractCursor
    mi::MethodInstance
end

update_mi(curs::AbstractCursor, ::MethodInstance) = error("""
missing `$AbstractCursor` API:
`$(typeof(curs))` is required to implement the `$update_mi(::$(typeof(curs)), ::MethodInstance) -> $(typeof(curs))` interface.
""")
update_mi(curs::CthulhuCursor, mi::MethodInstance) = CthulhuCursor(mi)

# This method is optional, but should be implemented if there is
# a sensible default cursor for a MethodInstance
AbstractCursor(interp::AbstractInterpreter, mi::MethodInstance) =
    CthulhuCursor(mi)

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
