"""
    AbstractCursor

Required overloads:
- `Cthulhu.lookup(interp::AbstractInterpreter, curs::AbstractCursor, optimize::Bool)`
- `Cthulhu.get_mi(curs::AbstractCursor) -> MethodInstance`
- `Cthulhu.update_cursor(curs::AbstractCursor, mi::MethodInstance)`
- `Cthulhu.navigate(curs::AbstractCursor, callsite::Callsite) -> AbstractCursor`
"""
abstract type AbstractCursor; end
struct CthulhuCursor <: AbstractCursor
    mi::MethodInstance
end

lookup(interp::AbstractInterpreter, curs::AbstractCursor, optimize::Bool) = error("""
missing `$AbstractCursor` API:
`$(typeof(curs))` is required to implement the `$lookup(::$(typeof(interp)), ::$(typeof(curs)), optimize::Bool)` interface.
""")
lookup(interp::CthulhuInterpreter, curs::CthulhuCursor, optimize::Bool) =
    lookup(interp, get_mi(curs), optimize)

get_mi(curs::AbstractCursor) = error("""
missing `$AbstractCursor` API:
`$(typeof(curs))` is required to implement the `$get_mi(::$(typeof(curs))) -> MethodInstance` interface.
""")
get_mi(curs::CthulhuCursor) = curs.mi

update_cursor(curs::AbstractCursor, ::MethodInstance) = error("""
missing `$AbstractCursor` API:
`$(typeof(curs))` is required to implement the `$update_cursor(::$(typeof(curs)), ::MethodInstance) -> $(typeof(curs))` interface.
""")
update_cursor(curs::CthulhuCursor, mi::MethodInstance) = CthulhuCursor(mi)

navigate(curs::AbstractCursor, callsite::Callsite) = error("""
missing `$AbstractCursor` API:
`$(typeof(curs))` is required to implement the `$navigate(::$(typeof(curs)), ::Callsite) -> AbstractCursor` interface.
""")
function navigate(curs::CthulhuCursor, callsite::Callsite)
    CthulhuCursor(get_mi(callsite))
end

# This method is optional, but should be implemented if there is
# a sensible default cursor for a MethodInstance
AbstractCursor(interp::AbstractInterpreter, mi::MethodInstance) =
    CthulhuCursor(mi)

function get_effects(interp::CthulhuInterpreter, mi::MethodInstance, opt::Bool)
    get_effects(opt ? interp.opt : interp.unopt, mi)
end
