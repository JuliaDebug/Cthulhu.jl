# A lightly-modified version of the same function in Base
# Highlights argument types with color specified by highlighter(typ)
function show_tuple_as_call(@nospecialize(highlighter), io::IO, name::Symbol, sig::Type, demangle=false, kwargs=nothing)
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
            isdefined(uw.name.module, uw.name.mt.name) &&
            ft == typeof(getfield(uw.name.module, uw.name.mt.name))
        print(env_io, (demangle ? demangle_function_name : identity)(uw.name.mt.name))
    elseif isa(ft, DataType) && ft.name === Type.body.name && !Core.Compiler.has_free_typevars(ft)
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
    if kwargs !== nothing
        print(io, "; ")
        first = true
        for (k, t) in kwargs
            first || print(io, ", ")
            first = false
            print(io, k, "::")
            show(io, t)
        end
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
function buildframes(bt)
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

# Extension API
backedges(mi::MethodInstance) = isdefined(mi, :backedges) ? mi.backedges : _emptybackedges
method(mi::MethodInstance) = mi.def
specTypes(mi::MethodInstance) = mi.specTypes
instance(mi::MethodInstance) = mi
nextnode(mi, edge) = edge

instance(sfs::Vector{StackTraces.StackFrame}) = sfs[end].linfo
method(sfs::Vector{StackTraces.StackFrame}) = method(instance(sfs))

instance(ipframes::Vector{IPFrames}) = instance(ipframes[1].sfs)
backedges(ipframes::Vector{IPFrames}) = (ret = ipframes[2:end]; isempty(ret) ? () : (ret,))

function callstring(io, mi)
    show_tuple_as_call(nonconcrete_red, IOContext(io, :color=>true), method(mi).name, specTypes(mi))
    return String(take!(io))
end
function callstring(io, sfs::Vector{StackTraces.StackFrame})
    for i = 1:length(sfs)-1
        sf = sfs[i]
        print(io, sf.func, " at ", sf.file, ':', sf.line, " => ")
    end
    sf = sfs[end]
    return callstring(io, instance(sfs)) * string(" at ", sf.file, ':', sf.line)
end
callstring(io, ipframes::Vector{IPFrames}) = callstring(io, ipframes[1].sfs)

if has_treemenu
    struct Data{T}
        callstr::String
        nd::T
    end

    function treelist(mi)
        io = IOBuffer()
        str = callstring(io, mi)
        treelist!(Node(Data(str, instance(mi))), io, mi, "", Base.IdSet{typeof(instance(mi))}())
    end
    function treelist!(parent::Node, io::IO, mi, indent::AbstractString, visited::Base.IdSet)
        mi ∈ visited && return parent
        push!(visited, instance(mi))
        indent *= " "
        for edge in backedges(mi)
            str = indent * callstring(io, edge)
            child = Node(Data(str, instance(edge)), parent)
            treelist!(child, io, nextnode(mi, edge), indent, visited)
        end
        return parent
    end
    treelist!(::Node, ::IO, ::Nothing, ::AbstractString, ::Base.IdSet) = nothing
else
    # TreeMenu can't be implemented, fallback to non-folding menu
    treelist(mi) = treelist!(String[], typeof(mi)[], IOBuffer(), mi, "", Base.IdSet{typeof(mi)}())

    function treelist!(strs, mis, io::IO, mi, indent::AbstractString, visited::Base.IdSet)
        mi ∈ visited && return strs, mis
        push!(visited, mi)
        str = indent * callstring(io, mi)
        push!(strs, str)
        push!(mis, mi)
        indent *= " "
        for edge in backedges(mi)
            treelist!(strs, mis, io, edge, indent, visited)
        end
        return strs, mis
    end
    treelist!(strs, mis, ::IO, ::Nothing, ::AbstractString, ::Base.IdSet) = nothing
end

treelist(bt::Vector{Union{Ptr{Nothing}, Base.InterpreterIP}}) = treelist(buildframes(bt))
