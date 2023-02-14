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

lookup(interp::AbstractInterpreter, curs::AbstractCursor, optimize::Bool) = error(lazy"""
missing `$AbstractCursor` API:
`$(typeof(curs))` is required to implement the `$lookup(interp::$(typeof(interp)), curs::$(typeof(curs)), optimize::Bool)` interface.
""")
lookup(interp::CthulhuInterpreter, curs::CthulhuCursor, optimize::Bool) =
    lookup(interp, get_mi(curs), optimize)

lookup_constproped(interp::AbstractInterpreter, curs::AbstractCursor, override::InferenceResult, optimize::Bool) = error(lazy"""
missing `$AbstractCursor` API:
`$(typeof(curs))` is required to implement the `$lookup_constproped(interp::$(typeof(interp)), curs::$(typeof(curs)), override::InferenceResult, optimize::Bool)` interface.
""")
lookup_constproped(interp::CthulhuInterpreter, ::CthulhuCursor, override::InferenceResult, optimize::Bool) =
    lookup_constproped(interp, override, optimize)

lookup_semiconcrete(interp::AbstractInterpreter, curs::AbstractCursor, override::SemiConcreteCallInfo, optimize::Bool) = error(lazy"""
missing `$AbstractCursor` API:
`$(typeof(curs))` is required to implement the `$lookup_semicocnrete(interp::$(typeof(interp)), curs::$(typeof(curs)), override::SemiConcreteCallInfo, optimize::Bool)` interface.
""")
lookup_semiconcrete(interp::CthulhuInterpreter, ::CthulhuCursor, override::SemiConcreteCallInfo, optimize::Bool) =
    lookup_semiconcrete(interp, override, optimize)

get_mi(curs::AbstractCursor) = error(lazy"""
missing `$AbstractCursor` API:
`$(typeof(curs))` is required to implement the `$get_mi(curs::$(typeof(curs))) -> MethodInstance` interface.
""")
get_mi(curs::CthulhuCursor) = curs.mi

get_optimized_codeinst(interp::AbstractInterpreter, curs::AbstractCursor) = error(lazy"""
missing `$AbstractCursor` API:
`$(typeof(curs))` is required to implement the `$get_optimized_codeinst(interp::$(typeof(interp)), curs::$(typeof(curs))) -> CodeInstance` interface.
""")
get_optimized_codeinst(interp::CthulhuInterpreter, curs::CthulhuCursor) = interp.opt[curs.mi]

update_cursor(curs::AbstractCursor, ::MethodInstance) = error(lazy"""
missing `$AbstractCursor` API:
`$(typeof(curs))` is required to implement the `$update_cursor(curs::$(typeof(curs)), mi::MethodInstance) -> $(typeof(curs))` interface.
""")
update_cursor(curs::CthulhuCursor, mi::MethodInstance) = CthulhuCursor(mi)

# TODO: This interface is incomplete, should probably also take a current cursor,
# or maybe be `CallSite based`
can_descend(interp::AbstractInterpreter, @nospecialize(key), optimize::Bool) = error(lazy"""
missing `$AbstractInterpreter` API:
`$(typeof(interp))` is required to implement the `$can_descend(interp::$(typeof(interp)), @nospecialize(key), optimize::Bool) -> Bool` interface.
""")
can_descend(interp::CthulhuInterpreter, @nospecialize(key), optimize::Bool) =
    haskey(optimize ? interp.opt : interp.unopt, key)

navigate(curs::AbstractCursor, callsite::Callsite) = error(lazy"""
missing `$AbstractCursor` API:
`$(typeof(curs))` is required to implement the `$navigate(curs::$(typeof(curs)), callsite::Callsite) -> AbstractCursor` interface.
""")
navigate(curs::CthulhuCursor, callsite::Callsite) = CthulhuCursor(get_mi(callsite))

get_remarks(::AbstractInterpreter, ::Union{MethodInstance,InferenceResult}) = nothing
get_remarks(interp::CthulhuInterpreter, key::Union{MethodInstance,InferenceResult}) = get(interp.remarks, key, nothing)
get_remarks(interp::AbstractInterpreter, key::SemiConcreteCallInfo) = PC2Remarks()

get_effects(::AbstractInterpreter, ::Union{MethodInstance,InferenceResult}) = nothing
get_effects(interp::CthulhuInterpreter, key::Union{MethodInstance,InferenceResult}) = get(interp.effects, key, nothing)
get_effects(interp::AbstractInterpreter, key::SemiConcreteCallInfo) = PC2Effects()

# This method is optional, but should be implemented if there is
# a sensible default cursor for a MethodInstance
AbstractCursor(interp::AbstractInterpreter, mi::MethodInstance) = CthulhuCursor(mi)

get_effects(interp::CthulhuInterpreter, mi::MethodInstance, opt::Bool) =
    get_effects(opt ? interp.opt : interp.unopt, mi)

mutable struct CustomToggle
    onoff::Bool
    key::UInt32
    toggle::Symbol
    description::String
    callback_on
    callback_off
    function CustomToggle(onoff::Bool, key, description,
        @nospecialize(callback_on), @nospecialize(callback_off))
        key = convert(UInt32, key)
        desc = convert(String, description)
        toggle = Symbol(desc)
        if haskey(TOGGLES, key)
            error(lazy"invalid Cthulhu API: key `$key` is already used.")
        elseif toggle in values(TOGGLES)
            error(lazy"invalid Cthulhu API: toggle `$toggle` is already used.")
        end
        return new(onoff, key, toggle, desc, callback_on, callback_off)
    end
end
custom_toggles(interp::AbstractInterpreter) = CustomToggle[]
