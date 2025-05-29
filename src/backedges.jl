# A lightly-modified version of the same function in Base
# Highlights argument types with color specified by highlighter(typ)

get_fname(@nospecialize(fT::DataType)) = @static VERSION ≥ v"1.13.0-DEV.647" ? fT.name.singletonname : fT.name.mt.name

function show_tuple_as_call(@nospecialize(highlighter), io::IO, name::Symbol, @nospecialize(sig::Type), demangle=false #=, kwargs=nothing =#)
    if sig === Tuple
        print(io, demangle ? Base.demangle_function_name(name) : name, "(...)")
        return
    end
    tv = Any[]
    env_io = io
    while isa(sig, UnionAll)
        push!(tv, sig.var)
        env_io = IOContext(env_io, :unionall_env => sig.var)
        sig = sig.body
    end
    sig = (sig::DataType).parameters

    ft = sig[1]
    uw = Base.unwrap_unionall(ft)
    if ft <: Function && isa(uw,DataType) && isempty(uw.parameters) &&
            isdefined(uw.name.module, get_fname(uw)) &&
            ft == typeof(getfield(uw.name.module, get_fname(uw)))
        print(env_io, (demangle ? Base.demangle_function_name : identity)(get_fname(uw)))
    elseif isa(ft, DataType) && ft.name === Type.body.name && !CC.has_free_typevars(ft)
        f = ft.parameters[1]
        print(env_io, f)
    else
        print(env_io, "(::", ft, ")")
    end

    first = true
    print(io, "(")
    for i = 2:length(sig)  # fixme (iter): `eachindex` with offset?
        first || print(io, ", ")
        first = false
        printstyled(env_io, "::", sig[i], color=highlighter(sig[i]))
    end
    print(io, ")")
    Base.show_method_params(io, tv)
    nothing
end

function stripType(@nospecialize(typ))
    if isa(typ, UnionAll)
        typ = Base.unwrap_unionall(typ)
    elseif isa(typ, TypeVar) || isa(typ, Union)
        return typ
    end
    Base.isvarargtype(typ) && return typ
    return typ <: Type && length(typ.parameters) == 1 ? typ.parameters[1] : typ
end
nonconcrete_red(@nospecialize(typ)) = isconcretetype(stripType(typ)) ? :nothing : :red

const _emptybackedges = MethodInstance[]

# For handling stacktraces with ascend
struct IPFrames
    sfs::Vector{StackTraces.StackFrame}
end
function buildframes(bt::Vector{Union{Ptr{Nothing}, Base.InterpreterIP}})
    ipframes = IPFrames[]
    for ip in bt
        sfs = Base.StackTraces.lookup(ip)
        sf = sfs[end]
        sf.from_c && continue
        mi = sf.linfo
        isa(mi, Core.MethodInstance) || continue
        push!(ipframes, IPFrames(sfs))
    end
    return ipframes
end
function buildframes(st::Vector{StackTraces.StackFrame})
    ipframes = IPFrames[]
    for sf in st
        mi = sf.linfo
        isa(mi, Core.MethodInstance) || continue
        push!(ipframes, IPFrames([sf]))
    end
    return ipframes
end

## Extension API

# With hindsight, this API was poorly designed and would be worth fixing in the next breaking release.
# See issue #582

"""
    edges = backedges(obj)

Return an iterable of children (callers) of `obj`. An `edge` should support [`Cthulhu.method`](@ref)
and [`Cthulhu.specTypes`](@ref), as well as [`Cthulhu.nextnode`](@ref) to get the next node object.

Part of the `ascend` API.
"""
function backedges end

"""
    inst = instance(obj)

Return the current node data corresponding to `obj`. `inst` should support [`Cthulhu.method`](@ref)
and [`Cthulhu.specTypes`](@ref).

Part of the `ascend` API.
"""
function instance end

"""
    m = method(edge)

Return the method corresponding to `edge`.

Part of the `ascend` API.
"""
function method end

"""
    sig = specTypes(edge)

Return the signature of `edge`.

Part of the `ascend` API.
"""
function specTypes end

"""
    childobj = nextnode(obj, edge)

Return the next node object from `obj` indicated by `edge`.

Part of the `ascend` API.
"""
function nextnode end


backedges(mi::MethodInstance) = isdefined(mi, :backedges) ? mi.backedges : _emptybackedges
method(mi::MethodInstance) = mi.def
specTypes(mi::MethodInstance) = mi.specTypes
instance(mi::MethodInstance) = mi
nextnode(mi, edge) = edge

instance(@nospecialize(tt::Type)) = tt

instance(sfs::Vector{StackTraces.StackFrame}) = isempty(sfs) ? CC.Timings.ROOTmi : sfs[end].linfo::MethodInstance # we checked this type condition within `buildframes`
method(sfs::Vector{StackTraces.StackFrame}) = method(instance(sfs))
backedges(sframes::Vector{StackTraces.StackFrame}) = (ret = sframes[2:end]; isempty(ret) ? () : (ret,))

instance(ipframes::Vector{IPFrames}) = isempty(ipframes) ? CC.Timings.ROOTmi : instance(ipframes[1].sfs)
backedges(ipframes::Vector{IPFrames}) = (ret = ipframes[2:end]; isempty(ret) ? () : (ret,))

function callstring(io::IO, edge)
    name = isa(method(edge), Method) ? method(edge).name : :toplevel
    show_tuple_as_call(nonconcrete_red, IOContext(io, :color=>true), name, specTypes(edge))
    return String(take!(io))
end
function callstring(io::IO, sfs::Vector{StackTraces.StackFrame})
    isempty(sfs) && return ""
    for i = 1:length(sfs)-1
        sf = sfs[i]
        print(io, sf.func, " at ", sf.file, ':', sf.line, " => ")
    end
    sf = sfs[end]
    return callstring(io, instance(sfs)) * string(" at ", sf.file, ':', sf.line)
end
callstring(io::IO, ipframes::Vector{IPFrames}) = isempty(ipframes) ? "" : callstring(io, ipframes[1].sfs)

struct Data{T}
    callstr::String
    nd::T
end

function treelist(mi)
    io = IOBuffer()
    imi = instance(mi)
    str = callstring(io, imi)
    treelist!(Node(Data(str, imi)), io, mi, "", Base.IdSet{typeof(imi)}())
end
function treelist!(parent::Node, io::IO, mi, indent::AbstractString, visited::Base.IdSet)
    imi = instance(mi)
    imi ∈ visited && return parent
    push!(visited, imi)
    indent *= " "
    for edge in backedges(mi)
        str = indent * callstring(io, edge)
        child = Node(typeof(parent.data)(str, instance(edge)), parent)
        treelist!(child, io, nextnode(mi, edge), indent, visited)
    end
    return parent
end
treelist!(::Node, ::IO, ::Nothing, ::AbstractString, ::Base.IdSet) = nothing

treelist(bt::Vector{Union{Ptr{Nothing}, Base.InterpreterIP}}) = treelist(buildframes(bt))
treelist(st::Vector{StackTraces.StackFrame}) = treelist(buildframes(st))
function treelist(exstk::Base.ExceptionStack)
    if length(exstk.stack) == 1
        return treelist(exstk.stack[1].backtrace)
    end
    # Don't error to avoid trashing `Main.err`
    @error "exception stack contains $(length(exstk.stack)) exceptions, pick one with `ascend(err.stack[i].backtrace)`"
    return nothing
end
