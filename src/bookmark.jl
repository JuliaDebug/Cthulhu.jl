"""
    Cthulhu.Bookmark

A `Cthulhu.Bookmark` remembers a method marked by `b` key during a descent.
It can be used with the following functions:

* `descend(::Bookmark)`, `descend_code_typed(::Bookmark)`,
  `descend_code_warntype(::Bookmark)`: continue the descent.
* `code_typed(::Bookmark)`, `code_warntype([::IO,] ::Bookmark)`: show typed IR
* `code_llvm([::IO,] ::Bookmark)`: pretty-print LLVM IR
* `code_native([::IO,] ::Bookmark)`: pretty-print native code
"""
struct Bookmark
    provider::AbstractProvider
    config::CthulhuConfig
    ci::CodeInstance
end
Bookmark(provider::AbstractProvider, ci::CodeInstance; config::CthulhuConfig = CONFIG) =
    Bookmark(provider, config, ci)

"""
    Cthulhu.BOOKMARKS :: Vector{Bookmark}

During a descent, methods can be "bookmarked" by pressing `b` key.  It
pushes a [`Cthulhu.Bookmark`](@ref) into `Cthulhu.BOOKMARKS`.  This can be
used to, e.g., continue descending by `descend(Cthulhu.BOOKMARKS[end])`.
See [`Cthulhu.Bookmark`](@ref) for other usages.
"""
const BOOKMARKS = Bookmark[]

# Turn off `optimize` and `debuginfo` for default `show` so that the
# output is smaller.
function Base.show(io::IO, ::MIME"text/plain", b::Bookmark; kwargs...)
    (; provider, config, ci) = b
    world = get_inference_world(b.provider)
    config = setproperties(b.config, NamedTuple(kwargs))
    state = CthulhuState(provider; ci, config)
    result = LookupResult(provider, ci, config.optimize)
    if get(io, :typeinfo, Any) === Bookmark  # a hack to check if in Vector etc.
        print(io, Callsite(-1, EdgeCallInfo(ci, result.rt, Effects()), :invoke))
        print(io, " (world: ", world, ")")
        return
    end
    println(io, Bookmark, " (world: $world):")
    cthulhu_typed(io, provider, state, result)
end

function Base.code_typed(b::Bookmark; kwargs...)
    (; provider, config, ci) = b
    config = setproperties(b.config, NamedTuple(kwargs))
    state = CthulhuState(provider; ci, config)
    result = LookupResult(provider, ci, config.optimize)
    return result.codeinf => result.rt
end

InteractiveUtils.code_warntype(b::Bookmark; kwargs...) = InteractiveUtils.code_warntype(stdout::IO, b; kwargs...)
InteractiveUtils.code_llvm(b::Bookmark; kwargs...) = InteractiveUtils.code_llvm(stdout::IO, b; kwargs...)
InteractiveUtils.code_native(b::Bookmark; kwargs...) = InteractiveUtils.code_native(stdout::IO, b; kwargs...)

function InteractiveUtils.code_warntype(io::IO, b::Bookmark; kwargs...)
    (; provider, config, ci) = b
    config = setproperties(b.config, NamedTuple(kwargs))
    state = CthulhuState(provider; ci, config)
    result = LookupResult(provider, ci, config.optimize)
    cthulhu_warntype(io, b.provider, state, result)
end

function InteractiveUtils.code_llvm(io::IO, b::Bookmark; dump_module = false, raw = false, kwargs...)
    (; provider, config, ci) = b
    config = setproperties(b.config, NamedTuple(kwargs))
    state = CthulhuState(provider; ci, config)
    result = LookupResult(provider, ci, config.optimize)
    cthulhu_llvm(io, provider, state, result; dump_module, raw)
end

function InteractiveUtils.code_native(io::IO, b::Bookmark; dump_module = false, raw = false, kwargs...)
    (; provider, config, ci) = b
    config = setproperties(b.config, NamedTuple(kwargs))
    state = CthulhuState(provider; ci, config)
    result = LookupResult(provider, ci, config.optimize)
    cthulhu_native(io, provider, state, result; dump_module, raw)
end
