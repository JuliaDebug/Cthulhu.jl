abstract type CallInfo end

struct Callsite
    id::Int # ssa-id
    info::CallInfo
    head::Symbol
end

function find_callsites end
function get_rt end
get_ci(c::Callsite) = get_ci(c.info)

show_callsite(io::IO, c::Callsite, info::CallInfo) = print_callsite_info(io, info)

function show_callinfo end
function print_callsite_info end

function TextWidthLimiter(@nospecialize(io::IO), c::Callsite)
    isa(io, TextWidthLimiter) && return io
    limit = get(io, :limit, false)::Bool
    cols = limit ? (displaysize(io)::Tuple{Int,Int})[2] : typemax(Int)
    limiter = TextWidthLimiter(io, cols)
    return limiter
end

function Base.show(@nospecialize(io::IO), c::Callsite)
    limiter = TextWidthLimiter(io, c)
    info = c.info
    if c.id != -1
        iswarn = get(io, :iswarn, false)::Bool
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
        limiter.width += 1 # for the '%' character
        print(limiter, c.id, " = ")
    end
    show_callsite(limiter, c, info)
    return nothing
end
