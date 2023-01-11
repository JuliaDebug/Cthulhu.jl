using Unicode

abstract type CallInfo end

# Call could be resolved to a singular MI
struct MICallInfo <: CallInfo
    mi::MethodInstance
    rt
    effects::Effects
    function MICallInfo(mi::MethodInstance, @nospecialize(rt), effects)
        if isa(rt, LimitedAccuracy)
            return LimitedCallInfo(new(mi, ignorelimited(rt), effects))
        else
            return new(mi, rt, effects)
        end
    end
end
get_mi(ci::MICallInfo) = ci.mi
get_rt(ci::CallInfo) = ci.rt
get_effects(ci::MICallInfo) = @static EFFECTS_ENABLED ? ci.effects : Effects()

abstract type WrappedCallInfo <: CallInfo end

get_wrapped(ci::WrappedCallInfo) = ci.wrapped
ignorewrappers(ci::CallInfo) = ci
ignorewrappers(ci::WrappedCallInfo) = ignorewrappers(get_wrapped(ci))
get_mi(ci::WrappedCallInfo) = get_mi(ignorewrappers(ci))
get_rt(ci::WrappedCallInfo) = get_rt(ignorewrappers(ci))
get_effects(ci::WrappedCallInfo) = get_effects(ignorewrappers(ci))

# only appears when inspecting pre-optimization states
struct LimitedCallInfo <: WrappedCallInfo
    wrapped::CallInfo
end

# uncached callsite, we can't recurse into this call
struct UncachedCallInfo <: WrappedCallInfo
    wrapped::CallInfo
end

struct PureCallInfo <: CallInfo
    argtypes::Vector{Any}
    rt
    PureCallInfo(argtypes::Vector{Any}, @nospecialize(rt)) =
        new(argtypes, rt)
end
get_mi(::PureCallInfo) = nothing
get_rt(pci::PureCallInfo) = pci.rt
get_effects(::PureCallInfo) = EFFECTS_TOTAL

# Failed
struct FailedCallInfo <: CallInfo
    sig
    rt
end
get_mi(ci::FailedCallInfo) = fail(ci)
get_rt(ci::FailedCallInfo) = fail(ci)
get_effects(ci::FailedCallInfo) = Effects()
function fail(ci::FailedCallInfo)
    @warn "MethodInstance extraction failed." ci.sig ci.rt
    return nothing
end

# Generated
struct GeneratedCallInfo <: CallInfo
    sig
    rt
end
get_mi(genci::GeneratedCallInfo) = fail(genci)
get_rt(genci::GeneratedCallInfo) = fail(genci)
get_effects(genci::GeneratedCallInfo) = Effects()
function fail(genci::GeneratedCallInfo)
    @warn "Can't extract MethodInstance from call to generated functions." genci.sig genci.rt
    return nothing
end

struct MultiCallInfo <: CallInfo
    sig
    rt
    callinfos::Vector{CallInfo}
end
# actual code-error
get_mi(ci::MultiCallInfo) = error("Can't extract MethodInstance from multiple call informations")
get_effects(mci::MultiCallInfo) = begin
    @static EFFECTS_ENABLED || return Effects()
    @static if isdefined(CC, :merge_effects)
        return mapreduce(get_effects, CC.merge_effects, mci.callinfos)
    else
        return mapreduce(get_effects, CC.tristate_merge, mci.callinfos)
    end
end

struct TaskCallInfo <: CallInfo
    ci::CallInfo
end
get_mi(tci::TaskCallInfo) = get_mi(tci.ci)
get_rt(tci::TaskCallInfo) = get_rt(tci.ci)
get_effects(tci::TaskCallInfo) = get_effects(tci.ci)

struct InvokeCallInfo <: CallInfo
    ci::CallInfo
    InvokeCallInfo(@nospecialize ci::CallInfo) = new(ci)
end
get_mi(ici::InvokeCallInfo) = get_mi(ici.ci)
get_rt(ici::InvokeCallInfo) = get_rt(ici.ci)
get_effects(ici::InvokeCallInfo) = get_effects(ici.ci)

# OpaqueClosure CallInfo
struct OCCallInfo <: CallInfo
    ci::CallInfo
    OCCallInfo(@nospecialize ci::CallInfo) = new(ci)
end
get_mi(occi::OCCallInfo) = get_mi(occi.ci)
get_rt(occi::OCCallInfo) = get_rt(occi.ci)
get_effects(occi::OCCallInfo) = get_effects(occi.ci)

# Special handling for ReturnTypeCall
struct ReturnTypeCallInfo <: CallInfo
    vmi::CallInfo # virtualized method call
end
get_mi((; vmi)::ReturnTypeCallInfo) = isa(vmi, FailedCallInfo) ? nothing : get_mi(vmi)
get_rt((; vmi)::ReturnTypeCallInfo) = Type{isa(vmi, FailedCallInfo) ? Union{} : widenconst(get_rt(vmi))}
get_effects(::ReturnTypeCallInfo) = @static EFFECTS_ENABLED ? EFFECTS_TOTAL : Effects()

struct ConstPropCallInfo <: CallInfo
    mi::CallInfo
    result::InferenceResult
end
get_mi(cpci::ConstPropCallInfo) = cpci.result.linfo
get_rt(cpci::ConstPropCallInfo) = get_rt(cpci.mi)
get_effects(cpci::ConstPropCallInfo) = get_effects(cpci.result)

struct ConcreteCallInfo <: CallInfo
    mi::CallInfo
    argtypes::ArgTypes
end
get_mi(ceci::ConcreteCallInfo) = get_mi(ceci.mi)
get_rt(ceci::ConcreteCallInfo) = get_rt(ceci.mi)
get_effects(ceci::ConcreteCallInfo) = get_effects(ceci.mi)

struct SemiConcreteCallInfo <: CallInfo
    mi::CallInfo
    ir::IRCode
end
get_mi(scci::SemiConcreteCallInfo) = get_mi(scci.mi)
get_rt(scci::SemiConcreteCallInfo) = get_rt(scci.mi)
get_effects(scci::SemiConcreteCallInfo) = get_effects(scci.mi)

# CUDA callsite
struct CuCallInfo <: CallInfo
     cumi::MICallInfo
end
get_mi(gci::CuCallInfo) = get_mi(gci.cumi)
get_rt(gci::CuCallInfo) = get_rt(gci.cumi)
get_effects(gci::CuCallInfo) = get_effects(gci.cumi)

struct Callsite
    id::Int # ssa-id
    info::CallInfo
    head::Symbol
end
get_mi(c::Callsite) = get_mi(c.info)
get_effects(c::Callsite) = get_effects(c.info)

# Callsite printing
mutable struct TextWidthLimiter <: IO
    io::IO
    width::Int
    limit::Int
end
TextWidthLimiter(io::IO, limit) = TextWidthLimiter(io, 0, limit)
Base.get(limiter::TextWidthLimiter, key, default) = get(limiter.io, key, default)

# Uncomment this to debug display of a TextWidthLimiter (e.g., triggered by `print(limited, args1, args...)`)
# Base.show(io::IO, twl::TextWidthLimiter) = error("do not display")

has_space(limiter::TextWidthLimiter, width::Int) = limiter.width + width < limiter.limit - 1
has_space(limiter::TextWidthLimiter, s) = has_space(limiter, textwidth(string(s)))
has_space(::IO, s) = true

function Base.print(io::TextWidthLimiter, s::String)
    io.width == io.limit && return
    width = textwidth(s)
    if has_space(io, width)
        print(io.io, s)
        io.width += width
        return
    end
    for c in graphemes(s)
        cwidth = textwidth(c)
        if has_space(io, cwidth)
            print(io.io, c)
            io.width += cwidth
        else
            break
        end
    end
    print(io.io, '…')
    io.width += 1
    return
end

function Base.print(io::TextWidthLimiter, c::Char)
    tw = textwidth(c)
    if has_space(io, tw)
        print(io.io, c)
        io.width += tw
        return
    end
    return
end

function Base.take!(io::TextWidthLimiter)
    io.width = 0
    return take!(io.io)
end

function headstring(@nospecialize(T))
    if isvarargtype(T)
        T = unwrapva(T)
    elseif isa(T, TypeVar)
        return string(T.name)
    end
    T = widenconst(T)
    if T isa Union || T === Union{}
        return string(T)::String
    elseif T isa UnionAll
        return headstring(Base.unwrap_unionall(T))
    else
        return string(T.name.name)::String
    end
end

function __show_limited(limiter, name, tt, @nospecialize(rt), effects)
    vastring(@nospecialize(T)) = (isvarargtype(T) ? headstring(T)*"..." : string(T)::String)

    if !has_space(limiter, name)
        print(limiter, '…')
        return
    end
    print(limiter, string(name))
    pstrings = String[vastring(T) for T in tt]
    headstrings = String[
        T isa DataType && isempty(T.parameters) ? headstring(T) : string(headstring(T), "{…}")
        for T in tt
    ]
    print(limiter, "(")
    if length(pstrings) != 0
        # See if we have space to print all the parameters fully
        if has_space(limiter, sum(textwidth, pstrings) + 3*length(pstrings))
            join(limiter, (string("::", T) for T in pstrings), ",")
        # Alright, see if we at least have enough space for each head
        elseif has_space(limiter, sum(textwidth, headstrings) + 3*length(pstrings))
            join(limiter, (string("::", T) for T in headstrings), ",")
        # Fine, what about just indicating the number of arguments
        elseif has_space(limiter, 2*(length(tt)))
            join(limiter, ("…" for _ in pstrings), ",")
        else
            print(limiter, "…")
        end
    end
    print(limiter, ")")

    # If we have space for the return type, print it
    rt_str = string(rt)::String
    if has_space(limiter, textwidth(rt_str)+2)
        print(limiter, "::", rt_str)
        with_effects = get(limiter, :with_effects, false)::Bool
        if with_effects
            # Additionally, if we have space for the effects, print it
            effects_str = string(effects)::String
            if has_space(limiter, textwidth(effects_str)+1)
                print(limiter, " ", effects)
            end
        end
    elseif has_space(limiter, 3)
        print(limiter, "::…")
    end

    return nothing
end

function show_callinfo(limiter, mici::MICallInfo)
    mi = mici.mi
    tt = (Base.unwrap_unionall(mi.specTypes)::DataType).parameters[2:end]
    name = (mi.def::Method).name
    rt = get_rt(mici)
    __show_limited(limiter, name, tt, rt, get_effects(mici))
end

function show_callinfo(limiter, ci::Union{MultiCallInfo, FailedCallInfo, GeneratedCallInfo})
    types = (ci.sig::DataType).parameters
    ft, tt = types[1], types[2:end]
    f = CC.singleton_type(ft)
    if f !== nothing
        name = "→ $f"
    elseif ft isa Union
        name = "→ (::Union{$(join(String[String(nameof(T)) for T in Base.uniontypes(ft)], ", "))})"
    else
        name = "→ (::$(nameof(ft)))"
    end
    __show_limited(limiter, name::String, tt, get_rt(ci), get_effects(ci))
end

function show_callinfo(limiter, pci::PureCallInfo)
    ft, tt... = pci.argtypes
    f = CC.singleton_type(ft)
    name = isnothing(f) ? "unknown" : string(f)
    __show_limited(limiter, name, tt, get_rt(pci), get_effects(pci))
end

function show_callinfo(limiter, ci::ConstPropCallInfo)
    # XXX: The first argument could be const-overriden too
    name = ci.result.linfo.def.name
    tt = ci.result.argtypes[2:end]
    ci = ignorewrappers(ci.mi)::MICallInfo
    __show_limited(limiter, name, tt, get_rt(ci), get_effects(ci))
end

function show_callinfo(limiter, ci::SemiConcreteCallInfo)
    # XXX: The first argument could be const-overriden too
    name = get_mi(ci).def.name
    tt = ci.ir.argtypes[2:end]
    __show_limited(limiter, name, tt, get_rt(ci), get_effects(ci))
end

function show_callinfo(limiter, ci::ConcreteCallInfo)
    # XXX: The first argument could be const-overriden too
    name = get_mi(ci).def.name
    tt = ci.argtypes[2:end]
    __show_limited(limiter, name, tt, get_rt(ci), get_effects(ci))
end

function show_callinfo(limiter, rci::ReturnTypeCallInfo)
    vmi = rci.vmi
    if isa(vmi, FailedCallInfo)
        ft = Base.tuple_type_head(vmi.sig)
        f = CC.singleton_type(ft)
        name = isnothing(f) ? "unknown" : string(f)
        tt = Base.tuple_type_tail(vmi.sig).parameters
        __show_limited(limiter, name, tt, vmi.rt, get_effects(vmi))
    else
        show_callinfo(limiter, vmi)
    end
end

function print_callsite_info(limiter::IO, info::WrappedCallInfo)
    wrapped_callinfo(limiter, info)
    show_callinfo(limiter, ignorewrappers(info))
end

function print_callsite_info(limiter::IO, info::PureCallInfo)
    print(limiter, "< pure > ")
    show_callinfo(limiter, info)
end

function print_callsite_info(limiter::IO, info::Union{MultiCallInfo, FailedCallInfo, GeneratedCallInfo})
    print(limiter, "call ")
    show_callinfo(limiter, info)
end

function print_callsite_info(limiter::IO, info::TaskCallInfo)
    print(limiter, "task < ")
    show_callinfo(limiter, info.ci)
    print(limiter, " >")
end

function print_callsite_info(limiter::IO, info::InvokeCallInfo)
    print(limiter, "invoke < ")
    show_callinfo(limiter, info.ci)
    print(limiter, " >")
end

function print_callsite_info(limiter::IO, info::ReturnTypeCallInfo)
    print(limiter, "return_type < ")
    show_callinfo(limiter, info)
    print(limiter, " >")
end

function print_callsite_info(limiter::IO, info::CuCallInfo)
    print(limiter, "cucall < ")
    show_callinfo(limiter, info.cumi)
    print(limiter, " >")
end

function print_callsite_info(limiter::IO, info::ConstPropCallInfo)
    print(limiter, "< constprop > ")
    show_callinfo(limiter, info)
end

function print_callsite_info(limiter::IO, info::SemiConcreteCallInfo)
    print(limiter, " = < semi-concrete eval > ")
    show_callinfo(limiter, info)
end

function print_callsite_info(limiter::IO, info::ConcreteCallInfo)
    print(limiter, "< concrete eval > ")
    show_callinfo(limiter, info)
end

function print_callsite_info(limiter::IO, info::OCCallInfo)
    print(limiter, "< opaque closure call > ")
    show_callinfo(limiter, info.ci)
end

const is_expected_union = @static isdefined(InteractiveUtils, :is_expected_union) ?
    InteractiveUtils.is_expected_union : Base.is_expected_union

function Base.show(io::IO, c::Callsite)
    limit = get(io, :limit, false)::Bool
    cols = limit ? (displaysize(io)::Tuple{Int,Int})[2] : typemax(Int)
    optimize = get(io, :optimize, true)::Bool
    iswarn = get(io, :iswarn, false)::Bool
    info = c.info
    rt = get_rt(info)
    if iswarn && is_type_unstable(rt)
        color = if rt isa Union && is_expected_union(rt)
            Base.warn_color()
        else
            Base.error_color()
        end
        printstyled(io, '%'; color)
    else
        print(io, '%')
    end
    limiter = TextWidthLimiter(io, cols)
    limiter.width += 1   # for the '%' character
    print(limiter, string(c.id))
    if isa(info, MICallInfo)
        print(limiter, optimize ? string(" = ", c.head, ' ') : " = ")
        show_callinfo(limiter, info)
    else
        print(limiter, " = ")
        print_callsite_info(limiter, info)
    end
    return nothing
end

function wrapped_callinfo(limiter, ci::WrappedCallInfo)
    print(limiter, " = < ")
    _wrapped_callinfo(limiter, ci)
    ci = get_wrapped(ci)
    while isa(ci, WrappedCallInfo)
        print(limiter, ", ")
        _wrapped_callinfo(limiter, ci)
        ci = get_wrapped(ci)
    end
    print(limiter, " > ")
end
_wrapped_callinfo(limiter, ::LimitedCallInfo)  = print(limiter, "limited")
_wrapped_callinfo(limiter, ::UncachedCallInfo) = print(limiter, "uncached")

# is_callsite returns true if `call` dispatches to `callee`
# See also `maybe_callsite` below
is_callsite(call::MethodInstance, callee::MethodInstance) = call === callee
is_callsite(::Nothing, callee::MethodInstance) = false   # for when `get_mi` returns `nothing`

# is_callsite for higher-level inputs
is_callsite(cs::Callsite, callee::MethodInstance) = is_callsite(cs.info, callee)
is_callsite(info::CallInfo, callee::MethodInstance) = is_callsite(get_mi(info), callee)
# special CallInfo cases:
function is_callsite(info::MultiCallInfo, callee::MethodInstance)
    for csi in info.callinfos
        is_callsite(csi, callee) && return true
    end
    return false
end

# maybe_callsite returns true if `call` *might* dispatch to `callee`
# See also `is_callsite` above
function maybe_callsite(call::MethodInstance, callee::MethodInstance)
    # handle comparison among Varargs
    function generalized_va_subtype(@nospecialize(Tshort), @nospecialize(Tlong))
        nshort, nlong = length(Tshort.parameters), length(Tlong.parameters)
        T = unwrapva(Tshort.parameters[end])
        T <: unwrapva(Tlong.parameters[end]) || return false
        for i = 1:nshort-1
            Tshort.parameters[i] <: Tlong.parameters[i] || return false
        end
        for i = nshort:nlong-1
            T <: Tlong.parameters[i] || return false
        end
        return T <: unwrapva(Tlong.parameters[end])
    end

    Tcall, Tcallee = call.specTypes, callee.specTypes
    Tcall <: Tcallee && return true
    # Make sure we handle Tcall = Tuple{Vararg{String}}, Tcallee = Tuple{String,Vararg{String}}
    if Base.isvatuple(Tcall) && Base.isvatuple(Tcallee)
        Tcall, Tcallee = Base.unwrap_unionall(Tcall), Base.unwrap_unionall(Tcallee)
        nargcall, nargcallee = length(Tcall.parameters), length(Tcallee.parameters)
        nargcall == nargcallee && return false
        return nargcall < nargcallee ? generalized_va_subtype(Tcall, Tcallee) : generalized_va_subtype(Tcallee, Tcall)
    end
    return false
end

# maybe_callsite for higher-level inputs
maybe_callsite(cs::Callsite, callee::MethodInstance) = maybe_callsite(cs.info, callee)
maybe_callsite(info::CallInfo, callee::MethodInstance) = maybe_callsite(get_mi(info), callee)
# Special CallInfo cases:
function maybe_callsite(info::MultiCallInfo, callee::MethodInstance)
    for csi in info.callinfos
        maybe_callsite(csi, callee) && return true
    end
    return false
end
maybe_callsite(info::PureCallInfo, mi::MethodInstance) = mi.specTypes <: Tuple{mapany(Core.Typeof ∘ unwrapconst, info.argtypes)...}

unwrapconst(@nospecialize(arg)) = arg isa Core.Const ? arg.val : arg
