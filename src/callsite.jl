using Unicode

abstract type CallInfo end

# Call could be resolved to a singular MI
struct EdgeCallInfo <: CallInfo
    edge::CodeInstance
    rt
    effects::Effects
    exct
    function EdgeCallInfo(edge::CodeInstance, @nospecialize(rt), effects::Effects, @nospecialize(exct=nothing))
        if isa(rt, LimitedAccuracy)
            return LimitedCallInfo(new(edge, ignorelimited(rt), effects, exct))
        else
            return new(edge, rt, effects, exct)
        end
    end
end
get_ci(ci::EdgeCallInfo) = ci.edge
get_rt(ci::EdgeCallInfo) = ci.rt
get_effects(ci::EdgeCallInfo) = ci.effects
get_exct(ci::EdgeCallInfo) = ci.exct

abstract type WrappedCallInfo <: CallInfo end

get_wrapped(ci::WrappedCallInfo) = ci.wrapped
ignorewrappers(ci::CallInfo) = ci
ignorewrappers(ci::WrappedCallInfo) = ignorewrappers(get_wrapped(ci))
get_ci(ci::WrappedCallInfo) = get_ci(ignorewrappers(ci))
get_rt(ci::WrappedCallInfo) = get_rt(ignorewrappers(ci))
get_effects(ci::WrappedCallInfo) = get_effects(ignorewrappers(ci))
get_exct(ci::WrappedCallInfo) = get_exct(ignorewrappers(ci))

# only appears when inspecting pre-optimization states
struct LimitedCallInfo <: WrappedCallInfo
    wrapped::CallInfo
end

# Runtime CallInfo
struct RTCallInfo <: CallInfo
    f
    argtyps
    rt
    exct
end
get_rt(ci::RTCallInfo) = ci.rt
get_ci(ci::RTCallInfo) = nothing
get_effects(ci::RTCallInfo) = Effects()
get_exct(ci::RTCallInfo) = ci.exct

struct PureCallInfo <: CallInfo
    argtypes::Vector{Any}
    rt
    PureCallInfo(argtypes::Vector{Any}, @nospecialize(rt)) =
        new(argtypes, rt)
end
get_ci(::PureCallInfo) = nothing
get_rt(pci::PureCallInfo) = pci.rt
get_effects(::PureCallInfo) = EFFECTS_TOTAL
get_exct(::PureCallInfo) = Union{}

# Failed
struct FailedCallInfo <: CallInfo
    sig
    rt
end
get_ci(ci::FailedCallInfo) = fail(ci)
get_rt(ci::FailedCallInfo) = fail(ci)
get_effects(ci::FailedCallInfo) = fail(ci)
get_exct(ci::FailedCallInfo) = fail(ci)
function fail(ci::FailedCallInfo)
    @warn "MethodInstance extraction failed." ci.sig ci.rt
    return nothing
end

# Generated
struct GeneratedCallInfo <: CallInfo
    sig
    rt
end
get_ci(genci::GeneratedCallInfo) = fail(genci)
get_rt(genci::GeneratedCallInfo) = fail(genci)
get_effects(genci::GeneratedCallInfo) = fail(genci)
get_exct(genci::GeneratedCallInfo) = fail(genci)
function fail(genci::GeneratedCallInfo)
    @warn "Can't extract MethodInstance from call to generated functions." genci.sig genci.rt
    return nothing
end

struct MultiCallInfo <: CallInfo
    sig
    rt
    exct
    callinfos::Vector{CallInfo}
    MultiCallInfo(@nospecialize(sig), @nospecialize(rt), callinfos::Vector{CallInfo},
                  @nospecialize(exct=nothing)) =
        new(sig, rt, exct, callinfos)
end
get_ci(ci::MultiCallInfo) = error("Can't extract MethodInstance from multiple call informations")
get_rt(ci::MultiCallInfo) = ci.rt
get_effects(mci::MultiCallInfo) = mapreduce(get_effects, CC.merge_effects, mci.callinfos)
get_exct(ci::MultiCallInfo) = ci.exct

struct TaskCallInfo <: CallInfo
    ci::CallInfo
end
get_ci(tci::TaskCallInfo) = get_ci(tci.ci)
get_rt(tci::TaskCallInfo) = get_rt(tci.ci)
get_effects(tci::TaskCallInfo) = get_effects(tci.ci)
get_exct(tci::TaskCallInfo) = get_exct(tci.ci)

struct InvokeCallInfo <: CallInfo
    ci::CallInfo
    InvokeCallInfo(@nospecialize ci::CallInfo) = new(ci)
end
get_ci(ici::InvokeCallInfo) = get_ci(ici.ci)
get_rt(ici::InvokeCallInfo) = get_rt(ici.ci)
get_effects(ici::InvokeCallInfo) = get_effects(ici.ci)
get_exct(ici::InvokeCallInfo) = get_exct(ici.ci)

# OpaqueClosure CallInfo
struct OCCallInfo <: CallInfo
    ci::CallInfo
    OCCallInfo(@nospecialize ci::CallInfo) = new(ci)
end
get_ci(occi::OCCallInfo) = get_ci(occi.ci)
get_rt(occi::OCCallInfo) = get_rt(occi.ci)
get_effects(occi::OCCallInfo) = get_effects(occi.ci)
get_exct(occi::OCCallInfo) = get_exct(occi.ci)

# Special handling for ReturnTypeCall
struct ReturnTypeCallInfo <: CallInfo
    vmi::CallInfo # virtualized method call
end
get_ci((; vmi)::ReturnTypeCallInfo) = isa(vmi, FailedCallInfo) ? nothing : get_ci(vmi)
get_rt((; vmi)::ReturnTypeCallInfo) = Type{isa(vmi, FailedCallInfo) ? Union{} : widenconst(get_rt(vmi))}
get_effects(::ReturnTypeCallInfo) = EFFECTS_TOTAL
get_exct(::ReturnTypeCallInfo) = Union{} # FIXME

struct ConstPropCallInfo <: CallInfo
    ci::CallInfo
    result::InferenceResult
end
get_ci(cpci::ConstPropCallInfo) = get_ci(cpci.ci)
get_rt(cpci::ConstPropCallInfo) = get_rt(cpci.ci)
get_effects(cpci::ConstPropCallInfo) = get_effects(cpci.result)
get_exct(cpci::ConstPropCallInfo) = get_exct(cpci.ci)

struct ConcreteCallInfo <: CallInfo
    ci::CallInfo
    argtypes::ArgTypes
end
get_ci(ceci::ConcreteCallInfo) = get_ci(ceci.ci)
get_rt(ceci::ConcreteCallInfo) = get_rt(ceci.ci)
get_effects(ceci::ConcreteCallInfo) = get_effects(ceci.ci)
get_exct(cici::ConcreteCallInfo) = get_exct(ceci.ci)

struct SemiConcreteCallInfo <: CallInfo
    ci::CallInfo
    ir::IRCode
end
get_ci(scci::SemiConcreteCallInfo) = get_ci(scci.ci)
get_rt(scci::SemiConcreteCallInfo) = get_rt(scci.ci)
get_effects(scci::SemiConcreteCallInfo) = get_effects(scci.ci)
get_exct(scci::SemiConcreteCallInfo) = get_exct(scci.ci)

# CUDA callsite
struct CuCallInfo <: CallInfo
    ci::EdgeCallInfo
end
get_ci(gci::CuCallInfo) = get_ci(gci.ci)
get_rt(gci::CuCallInfo) = get_rt(gci.ci)
get_effects(gci::CuCallInfo) = get_effects(gci.ci)

struct CthulhuCallInfo <: CCCallInfo
    meta::CallMeta
end
CC.add_edges_impl(edges::Vector{Any}, info::CthulhuCallInfo) = CC.add_edges!(edges, info.meta.info)
CC.nsplit_impl(info::CthulhuCallInfo) = CC.nsplit(info.meta.info)
CC.getsplit_impl(info::CthulhuCallInfo, idx::Int) = CC.getsplit(info.meta.info, idx)
CC.getresult_impl(info::CthulhuCallInfo, idx::Int) = CC.getresult(info.meta.info, idx)

struct Callsite
    id::Int # ssa-id
    info::CallInfo
    head::Symbol
end
get_ci(c::Callsite) = get_ci(c.info)
get_effects(c::Callsite) = get_effects(c.info)

# Callsite printing

# compatibility (TextWidthLimiter used to live here)
has_space(limiter::TextWidthLimiter, width::Int) = limiter.width + width < limiter.limit - 1
has_space(limiter::TextWidthLimiter, s) = has_space(limiter, textwidth(string(s)))
has_space(::IO, s) = true

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

function __show_limited(limiter, name, tt, @nospecialize(rt), effects, @nospecialize(exct=nothing))
    vastring(@nospecialize(T)) = (isvarargtype(T) ? headstring(T)*"..." : string(T)::String)

    # If effects are explicitly turned on, make sure to print them, even
    # if there otherwise isn't space for them, since the effects are the
    # most important piece of information if turned on.
    show_effects = get(limiter, :effects, false)::Bool
    exception_type = get(limiter, :exception_type, false)::Bool && exct !== nothing

    if isa(limiter, TextWidthLimiter)
        show_effects && (limiter.width += textwidth(repr(effects)) + 1)
        exception_type && (limiter.width += textwidth(string(exct)) + 1)
        limiter.limit = max(limiter.width, limiter.limit)
    end

    if !has_space(limiter, name)
        print(limiter, '…')
        @goto print_effects
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
    elseif has_space(limiter, 3)
        print(limiter, "::…")
    end

    @label print_effects
    if show_effects
        # Print effects unlimited
        print(limiter.io, " ", effects)
    end
    if exception_type
        print(limiter.io, ' ', ExctWrapper(exct))
    end

    return nothing
end

struct ExctWrapper
    exct
    ExctWrapper(@nospecialize exct) = new(exct)
end

function Base.show(io::IO, (;exct)::ExctWrapper)
    color = exct === Union{} ? :green : :yellow
    printstyled(io, "(↑::", exct, ")"; color)
end

function show_callinfo(limiter, ci::EdgeCallInfo)
    mi = ci.edge.def
    tt = (Base.unwrap_unionall(mi.specTypes)::DataType).parameters[2:end]
    if !isa(mi.def, Method)
        name = ":toplevel"
    else
        name = mi.def.name
    end
    rt = get_rt(ci)
    exct = get_exct(ci)
    __show_limited(limiter, name, tt, rt, get_effects(ci), exct)
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

show_callinfo(limiter, ci::RTCallInfo) = __show_limited(limiter, "$(ci.f)", ci.argtyps, get_rt(ci), get_effects(ci))

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
    ci = ignorewrappers(ci.ci)::EdgeCallInfo
    __show_limited(limiter, name, tt, get_rt(ci), get_effects(ci))
end

function show_callinfo(limiter, ci::SemiConcreteCallInfo)
    # XXX: The first argument could be const-overriden too
    name = get_ci(ci).def.def.name
    tt = ci.ir.argtypes[2:end]
    __show_limited(limiter, name, tt, get_rt(ci), get_effects(ci))
end

function show_callinfo(limiter, ci::ConcreteCallInfo)
    # XXX: The first argument could be const-overriden too
    name = get_ci(ci).def.def.name
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

function print_callsite_info(limiter::IO, info::RTCallInfo)
    print(limiter, "runtime < ")
    show_callinfo(limiter, info)
    print(limiter, " >")
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

const is_expected_union = InteractiveUtils.is_expected_union

function Base.show(io::IO, c::Callsite)
    limit = get(io, :limit, false)::Bool
    cols = limit ? (displaysize(io)::Tuple{Int,Int})[2] : typemax(Int)
    optimize = get(io, :optimize, true)::Bool
    iswarn = get(io, :iswarn, false)::Bool
    info = c.info
    rt = get_rt(info)
    limiter = TextWidthLimiter(io, cols)
    if c.id != -1
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
        limiter.width += 1 # for the '%' character
        c.id != -1 && print(limiter, c.id, " = ")
    end
    if isa(info, EdgeCallInfo)
        optimize && print(limiter, c.head, ' ')
        show_callinfo(limiter, info)
    else
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

# is_callsite returns true if `call` dispatches to `callee`
# See also `maybe_callsite` below
is_callsite(call::CodeInstance, callee::MethodInstance) = call.def === callee
is_callsite(::Nothing, callee::MethodInstance) = false   # for when `get_ci` returns `nothing`

# is_callsite for higher-level inputs
is_callsite(cs::Callsite, callee::MethodInstance) = is_callsite(cs.info, callee)
is_callsite(info::CallInfo, callee::MethodInstance) = is_callsite(get_ci(info), callee)
# special CallInfo cases:
function is_callsite(info::MultiCallInfo, callee::MethodInstance)
    for csi in info.callinfos
        is_callsite(csi, callee) && return true
    end
    return false
end

# maybe_callsite returns true if `call` *might* dispatch to `callee`
# See also `is_callsite` above
function maybe_callsite(call::CodeInstance, callee::MethodInstance)
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

    Tcall, Tcallee = call.def.specTypes, callee.specTypes
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
maybe_callsite(cs::Callsite, @nospecialize(tt::Type)) = maybe_callsite(cs.info, tt)
maybe_callsite(info::CallInfo, callee::MethodInstance) = maybe_callsite(get_ci(info), callee)
# Special CallInfo cases:
function maybe_callsite(info::MultiCallInfo, callee::MethodInstance)
    for csi in info.callinfos
        maybe_callsite(csi, callee) && return true
    end
    return false
end
maybe_callsite(info::PureCallInfo, mi::MethodInstance) = mi.specTypes <: Tuple{mapany(Core.Typeof ∘ unwrapconst, info.argtypes)...}
maybe_callsite(info::RTCallInfo, mi::MethodInstance) = false

function maybe_callsite(info::RTCallInfo, @nospecialize(tt::Type))
    isa(tt, Union) && return maybe_callsite(info, tt.a) || maybe_callsite(info, tt.b)
    isa(tt, DataType) || return false
    typeof(info.f) === tt.parameters[1] || return false
    for (a, b) in zip(info.argtyps, tt.parameters[2:end])
        a === b || return false
    end
    return true
end
function maybe_callsite(info::EdgeCallInfo, @nospecialize(tt::Type))
    return tt <: info.mi.specTypes
end

maybe_callsite(info::CallInfo, @nospecialize(tt::Type)) = false

unwrapconst(@nospecialize(arg)) = arg isa Core.Const ? arg.val : arg
