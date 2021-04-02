using Unicode

abstract type CallInfo; end

# Call could be resolved to a singular MI
struct MICallInfo <: CallInfo
    mi::MethodInstance
    rt
    function MICallInfo(mi::MethodInstance, @nospecialize(rt))
        if isa(rt, LimitedAccuracy)
            return LimitedCallInfo(new(mi, ignorelimited(rt)))
        else
            return new(mi, rt)
        end
    end
end
get_mi(ci::MICallInfo) = ci.mi

# only appears when inspecting pre-optimization states
struct LimitedCallInfo <: CallInfo
    ci::CallInfo
end
get_mi(ci::LimitedCallInfo) = get_mi(ci.ci)

# uncached callsite, we can't recurse into this call
struct UncachedCallInfo <: CallInfo
    ci::CallInfo
end
get_mi(ci::UncachedCallInfo) = get_mi(ci.ci)

# Failed
struct FailedCallInfo <: CallInfo
    sig
    rt
end

function get_mi(ci::FailedCallInfo)
    @error "MethodInstance extraction failed" ci.sig ci.rt
    return nothing
end

# Generated
struct GeneratedCallInfo <: CallInfo
    sig
    rt
end
function get_mi(genci::GeneratedCallInfo)
    @error "Can't extract MethodInstance from call to generated functions" genci.sig genci.rt
    return nothing
end

struct MultiCallInfo <: CallInfo
    sig
    rt
    callinfos::Vector{CallInfo}
end
# actual code-error
get_mi(ci::MultiCallInfo) = error("Can't extract MethodInstance from multiple call informations")

struct DeoptimizedCallInfo <: CallInfo
    accurate::CallInfo
    deoptimized::CallInfo
end
get_mi(ci::DeoptimizedCallInfo) = get_mi(ci.accurate)

struct TaskCallInfo <: CallInfo
    ci::CallInfo
end
get_mi(tci::TaskCallInfo) = get_mi(tci.ci)

# OpaqueClosure CallInfo
struct OCCallInfo <: CallInfo
    ci::MICallInfo
end
OCCallInfo(mi::MethodInstance, rt) = OCCallInfo(MICallInfo(mi, rt))
get_mi(tci::OCCallInfo) = get_mi(tci.ci)

# Special handling for ReturnTypeCall
struct ReturnTypeCallInfo <: CallInfo
    called_mi::CallInfo
end
get_mi(rtci::ReturnTypeCallInfo) = get_mi(rtci.called_mi)

struct ConstPropCallInfo <: CallInfo
    mi::CallInfo
    result::InferenceResult
end
get_mi(cpci::ConstPropCallInfo) = cpci.result.linfo

# CUDA callsite
struct CuCallInfo <: CallInfo
     cumi::MICallInfo
end
get_mi(gci::CuCallInfo) = get_mi(gci.cumi)

struct Callsite
    id::Int # ssa-id
    info::CallInfo
    head::Symbol
end
get_mi(c::Callsite) = get_mi(c.info)

# Callsite printing
mutable struct TextWidthLimiter
    io::IO
    width::Int
    limit::Int
end
TextWidthLimiter(io::IO, limit) = TextWidthLimiter(io, 0, limit)
has_space(limiter::TextWidthLimiter, width::Int) = limiter.width + width < limiter.limit - 1
has_space(limiter::TextWidthLimiter, s) = has_space(limiter, textwidth(string(s)))
function Base.print(io::TextWidthLimiter, s::String)
    io.width == io.limit && return 0
    width = textwidth(s::String)
    if has_space(io, width)
        print(io.io, s)
        io.width += width
        return
    else
        for c in graphemes(s)
            cwidth = textwidth(c)
            if has_space(io, cwidth)
                print(io, c)
                io.width += cwidth
            else
                break
            end
        end
        print(io, '…')
        io.width += 1
    end
end

function headstring(@nospecialize(T))
    T = widenconst(Base.unwrapva(T))
    if T isa Union || T === Union{}
        return string(T)
    elseif T isa UnionAll
        return headstring(Base.unwrap_unionall(T))
    else
        return string(T.name)
    end
end


function __show_limited(limiter, name, tt, rt)
    if !has_space(limiter, name)
        print(limiter, '…')
        return
    end
    print(limiter, string(name))
    pstrings = map(string, tt)
    headstrings = map(headstring, tt)
    print(limiter, "(")
    if length(pstrings) != 0
        # See if we have space to print all the parameters fully
        if has_space(limiter, sum(textwidth, pstrings) + 3*length(pstrings))
            print(limiter, join(map(T->string("::", T), pstrings), ","))
        # Alright, see if we at least have enough space for each head
        elseif has_space(limiter, sum(textwidth, headstrings) + 6*length(pstrings))
            print(limiter, join(map(T->string("::", T, "{…}"), headstrings), ","))
        # Fine, what about just indicating the number of arguments
        elseif has_space(limiter, 2*(length(tt)))
            print(limiter, join(map(T->"…", pstrings), ","))
        else
            print(limiter, "…")
        end
    end
    print(limiter, ")")

    # If we have space for the return type, print it
    rts = string(rt)
    if has_space(limiter, textwidth(rts)+2)
        print(limiter, string("::", rts))
    end
end

function show_callinfo(limiter, mici::MICallInfo)
    mi = mici.mi
    tt = (Base.unwrap_unionall(mi.specTypes)::DataType).parameters[2:end]
    name = mi.def.name
    rt = mici.rt
    __show_limited(limiter, name, tt, rt)
end

function show_callinfo(limiter, ci::Union{MultiCallInfo, FailedCallInfo, GeneratedCallInfo})
    types = (ci.sig::DataType).parameters
    f = types[1]
    if f isa Union
        name = string(f)
    else
        name = nameof(f)
    end
    tt = types[2:end]
    rt = ci.rt
    __show_limited(limiter, name, tt, rt)
end

function show_callinfo(limiter, ci::ConstPropCallInfo)
    # XXX: The first argument could be const-overriden too
    name = ci.result.linfo.def.name
    tt = ci.result.argtypes[2:end]
    __show_limited(limiter, name, tt, ci.mi.rt)
end

function Base.show(io::IO, c::Callsite)
    limit = get(io, :limit, false)::Bool
    cols = limit ? (displaysize(io)::Tuple{Int,Int})[2] : typemax(Int)
    optimize = get(io, :optimize, true)::Bool
    limiter = TextWidthLimiter(io, cols)
    print(limiter, string("%", c.id, " "))
    info = c.info
    if isa(info, MICallInfo)
        optimize ? print(limiter, string(" = ", c.head, ' ')) : print(limiter, " = ")
        show_callinfo(limiter, info)
    elseif isa(info, LimitedCallInfo)
        print(limiter, " = < limited > ")
        show_callinfo(limiter, info.ci)
    elseif isa(info, UncachedCallInfo)
        print(limiter, " = < uncached > ")
        show_callinfo(limiter, info.ci)
    elseif info isa MultiCallInfo
        print(limiter, " = call ")
        show_callinfo(limiter, info)
    elseif isa(info, DeoptimizedCallInfo)
        deoptstr = sprint(context = IOContext(io, :color => true)) do tmpio
            print(tmpio, " = ")
            printstyled(tmpio, "deoptimized"; color = :red)
            print(tmpio, " ")
        end
        print(limiter, deoptstr)
        show_callinfo(limiter, info.accurate)
    elseif info isa FailedCallInfo ||
           info isa GeneratedCallInfo
        print(limiter, " = call ")
        show_callinfo(limiter, info)
    elseif info isa TaskCallInfo
        print(limiter, " = task < ")
        show_callinfo(limiter, info.ci)
        print(limiter, " >")
    elseif isa(info, ReturnTypeCallInfo)
        print(limiter, " = return_type < ")
        show_callinfo(limiter, info.called_mi)
        print(limiter, " >")
    elseif isa(info, CuCallInfo)
        print(limiter, " = cucall < ")
        show_callinfo(limiter, info.cumi)
        print(limiter, " >")
    elseif isa(info, ConstPropCallInfo)
        print(limiter, " = < constprop > ")
        show_callinfo(limiter, info)
    elseif isa(info, OCCallInfo)
        print(limiter, " = < opaque closure call > ")
        show_callinfo(limiter, info.ci)
    end
end

is_callsite(cs::Callsite, mi::MethodInstance) = is_callsite(cs.info, mi)
is_callsite(info::MICallInfo, mi::MethodInstance) = info.mi == mi
is_callsite(info::LimitedCallInfo, mi::MethodInstance) = is_callsite(info.ci, mi)
is_callsite(info::UncachedCallInfo, mi::MethodInstance) = is_callsite(info.ci, mi)
is_callsite(info::ConstPropCallInfo, mi::MethodInstance) = is_callsite(info.mi, mi)
is_callsite(info::DeoptimizedCallInfo, mi::MethodInstance) = is_callsite(info.accurate, mi)
is_callsite(info::TaskCallInfo, mi::MethodInstance) = is_callsite(info.ci, mi)
is_callsite(info::ReturnTypeCallInfo, mi::MethodInstance) = is_callsite(info.called_mi, mi)
is_callsite(info::CuCallInfo, mi::MethodInstance) = is_callsite(info.cumi, mi)
function is_callsite(info::MultiCallInfo, mi::MethodInstance)
    for csi in info.callinfos
        is_callsite(csi, mi) && return true
    end
    return false
end
is_callsite(::CallInfo, mi::MethodInstance) = false
