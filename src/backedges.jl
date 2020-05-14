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
    sig = sig.parameters

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
    elseif isa(typ, TypeVar)
        return typ
    end
    return typ <: Type ? typ.parameters[1] : typ
end
nonconcrete_red(@nospecialize(typ)) = isconcretetype(stripType(typ)) ? :nothing : :red

treelist(mi::MethodInstance) = treelist!(String[], MethodInstance[], IOBuffer(), mi, "", Base.IdSet{MethodInstance}())

function treelist!(strs, mis, io::IO, mi::MethodInstance, indent::AbstractString, visited::Base.IdSet)
    mi ∈ visited && return strs, mis
    push!(visited, mi)
    show_tuple_as_call(nonconcrete_red, IOContext(io, :color=>true), mi.def.name, mi.specTypes)
    push!(strs, indent * String(take!(io)))
    push!(mis, mi)
    if isdefined(mi, :backedges)
        indent *= " "
        for edge in mi.backedges
            treelist!(strs, mis, io, edge, indent, visited)
        end
    end
    return strs, mis
end
