using Core.IR
using Cthulhu: Cthulhu
using Base.Meta: isexpr
using InteractiveUtils: gen_call_with_extracted_types_and_kwargs

argextype(@nospecialize args...) = Cthulhu.CC.argextype(args...)
argextype(@nospecialize(x), src::CodeInfo) = argextype(x, src, Cthulhu.CC.VarState[])
code_typed1(args...; kwargs...) = first(only(code_typed(args...; kwargs...)))::CodeInfo
macro code_typed1(ex0...)
    return gen_call_with_extracted_types_and_kwargs(__module__, :code_typed1, ex0)
end
get_code(args...; kwargs...) = code_typed1(args...; kwargs...).code
macro get_code(ex0...)
    return gen_call_with_extracted_types_and_kwargs(__module__, :get_code, ex0)
end

# check if `x` is a statement with a given `head`
isnew(@nospecialize x) = isexpr(x, :new)
isreturn(@nospecialize x) = isa(x, ReturnNode)

# check if `x` is a dynamic call of a given function
iscall(y) = @nospecialize(x) -> iscall(y, x)
function iscall((src, f), @nospecialize(x))
    return iscall(x) do @nospecialize x
        Cthulhu.CC.singleton_type(argextype(x, src)) === f
    end
end
function iscall(pred::Base.Callable, @nospecialize(x))
    if isexpr(x, :(=))
        x = x.args[2]
    end
    return isexpr(x, :call) && pred(x.args[1])
end

# check if `x` is a statically-resolved call of a function whose name is `sym`
isinvoke(y) = @nospecialize(x) -> isinvoke(y, x)
isinvoke(sym::Symbol, @nospecialize(x)) = isinvoke(mi->mi.def.name===sym, x)
isinvoke(pred::Function, @nospecialize(x)) = isexpr(x, :invoke) && pred(x.args[1]::CodeInstance)

function fully_eliminated(@nospecialize args...; retval=(@__FILE__), kwargs...)
    code = code_typed1(args...; kwargs...).code
    if retval !== (@__FILE__)
        length(code) == 1 || return false
        code1 = code[1]
        isreturn(code1) || return false
        val = code1.val
        if val isa QuoteNode
            val = val.value
        end
        return val == retval
    else
        return length(code) == 1 && isreturn(code[1])
    end
end
