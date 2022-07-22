"""
    AbstractCursor

Required overloads:
- `Cthulhu.lookup(interp::AbstractInterpreter, curs::AbstractCursor, optimize::Bool)`
- `Cthulhu.lookup_constproped(interp::AbstractInterpreter, curs::AbstractCursor, override::InferenceResult, optimize::Bool)`
- `Cthulhu.get_mi(curs::AbstractCursor) -> MethodInstance`
- `Cthulhu.get_optimized_codeinst(interp::AbstractInterpreter, curs::AbstractCursor) -> CodeInstance`
- `Cthulhu.update_cursor(curs::AbstractCursor, mi::MethodInstance)`
- `Cthulhu.navigate(curs::AbstractCursor, callsite::Callsite) -> AbstractCursor`
"""
abstract type AbstractCursor; end
struct CthulhuCursor <: AbstractCursor
    mi::MethodInstance
end

lookup(interp::AbstractInterpreter, curs::AbstractCursor, optimize::Bool) = error("""
missing `$AbstractCursor` API:
`$(typeof(curs))` is required to implement the `$lookup(::$(typeof(interp)), ::$(typeof(curs)), ::Bool)` interface.
""")
lookup(interp::CthulhuInterpreter, curs::CthulhuCursor, optimize::Bool) =
    lookup(interp, get_mi(curs), optimize)

lookup_constproped(interp::AbstractInterpreter, curs::AbstractCursor, override::InferenceResult, optimize::Bool) = error("""
missing `$AbstractCursor` API:
`$(typeof(curs))` is required to implement the `$lookup_constproped(::$(typeof(interp)), ::$(typeof(curs)), ::InferenceResult, ::Bool)` interface.
""")
lookup_constproped(interp::CthulhuInterpreter, ::CthulhuCursor, override::InferenceResult, optimize::Bool) =
    lookup_constproped(interp, override, optimize)

get_mi(curs::AbstractCursor) = error("""
missing `$AbstractCursor` API:
`$(typeof(curs))` is required to implement the `$get_mi(::$(typeof(curs))) -> MethodInstance` interface.
""")
get_mi(curs::CthulhuCursor) = curs.mi

get_optimized_codeinst(interp::AbstractInterpreter, curs::AbstractCursor) = error("""
missing `$AbstractCursor` API:
`$(typeof(curs))` is required to implement the `$get_optimized_codeinst(::$(typeof(curs))) -> CodeInstance` interface.
""")
get_optimized_codeinst(interp::CthulhuInterpreter, curs::CthulhuCursor) = interp.opt[curs.mi]

update_cursor(curs::AbstractCursor, ::MethodInstance) = error("""
missing `$AbstractCursor` API:
`$(typeof(curs))` is required to implement the `$update_cursor(::$(typeof(curs)), ::MethodInstance) -> $(typeof(curs))` interface.
""")
update_cursor(curs::CthulhuCursor, mi::MethodInstance) = CthulhuCursor(mi)

# TODO: This interface is incomplete, should probably also take a current cursor,
# or maybe be `CallSite based`
can_descend(interp::AbstractInterpreter, ::Any, optimize::Bool) = error("""
missing `$AbstractCursor` API:
`$(typeof(curs))` is required to implement the `$can_descend(::$(typeof(interp)), ::Any, Bool) -> Bool` interface.
""")
can_descend(interp::CthulhuInterpreter, @nospecialize(key), optimize::Bool) =
    haskey(optimize ? interp.opt : interp.unopt, key)


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
