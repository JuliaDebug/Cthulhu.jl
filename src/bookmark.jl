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

function CthulhuState(bookmark::Bookmark; terminal=default_terminal(), kwargs...)
    config = set_config(bookmark.config; kwargs...)
    state = CthulhuState(bookmark.provider; terminal, config, bookmark.ci)
    return state
end

"""
    Cthulhu.BOOKMARKS :: Vector{Bookmark}

During a descent, state can be "bookmarked" by pressing `b`, which pushes a [`Cthulhu.Bookmark`](@ref) into `Cthulhu.BOOKMARKS`. This can be used to, e.g., continue descending with `descend(Cthulhu.BOOKMARKS[end])`.

See [`Cthulhu.Bookmark`](@ref) for other usages.
"""
const BOOKMARKS = Bookmark[]

function Base.show(io::IO, ::MIME"text/plain", bookmark::Bookmark; kwargs...)
    (; provider, ci) = bookmark
    state = CthulhuState(bookmark; kwargs...)
    result = LookupResult(provider, ci, state.config.optimize)
    world = get_inference_world(provider)
    if get(io, :typeinfo, Any) === Bookmark  # a hack to check if in Vector etc.
        info = EdgeCallInfo(ci, result.rt, Effects())
        callsite = Callsite(-1, info, :invoke)
        print(io, callsite)
        print(io, " (world: ", world, ")")
        return
    end
    println(io, Bookmark, " (world: $world):")
    view_function(state)(io, provider, state, result)
end

function Base.code_typed(bookmark::Bookmark; kwargs...)
    (; provider, ci) = bookmark
    state = CthulhuState(bookmark; kwargs...)
    result = LookupResult(provider, ci, state.config.optimize)
    src = something(result.src, result.ir)::Union{CodeInfo, IRCode}
    return src => result.rt
end

InteractiveUtils.code_warntype(bookmark::Bookmark; kwargs...) =
    InteractiveUtils.code_warntype(stdout::IO, bookmark; kwargs...)
InteractiveUtils.code_llvm(bookmark::Bookmark; kwargs...) =
    InteractiveUtils.code_llvm(stdout::IO, bookmark; kwargs...)
InteractiveUtils.code_native(bookmark::Bookmark; kwargs...) =
    InteractiveUtils.code_native(stdout::IO, bookmark; kwargs...)

function InteractiveUtils.code_warntype(io::IO, bookmark::Bookmark; kwargs...)
    (; provider, ci) = bookmark
    state = CthulhuState(bookmark; kwargs...)
    result = LookupResult(provider, ci, state.config.optimize)
    cthulhu_warntype(io, provider, state, result)
end

function InteractiveUtils.code_llvm(io::IO, bookmark::Bookmark; dump_module = false, raw = false, kwargs...)
    (; provider, ci) = bookmark
    state = CthulhuState(bookmark; kwargs...)
    result = LookupResult(provider, ci, state.config.optimize)
    cthulhu_llvm(io, provider, state, result; dump_module, raw)
end

function InteractiveUtils.code_native(io::IO, bookmark::Bookmark; dump_module = false, raw = false, kwargs...)
    (; provider, ci) = bookmark
    state = CthulhuState(bookmark; kwargs...)
    result = LookupResult(provider, ci, state.config.optimize)
    cthulhu_native(io, provider, state, result; dump_module, raw)
end
