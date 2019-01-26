struct Callsite
    id::Int # ssa-id
    mi::MethodInstance
    rt
end


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

function Base.show(io::IO, c::Callsite)
    limit = get(io, :limit, false)
    cols = limit ? displaysize(io)[2] : typemax(Int)
    limiter = TextWidthLimiter(io, cols)
    print(limiter, string("%", c.id, " = invoke "))
    if !has_space(limiter, c.mi.def.name)
        print(limiter, '…')
        return
    end
    print(limiter, string(c.mi.def.name))
    tt = c.mi.specTypes.parameters[2:end]
    pstrings = map(string, tt)
    headstrings = map(x->isa(x, Union) ? string(x) : string(Base.unwrap_unionall(x).name), tt)
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
    rts = string(c.rt)
    if has_space(limiter, textwidth(rts)+2)
        print(limiter, string("::", rts))
    end
end
