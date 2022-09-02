# use annonymous module since this file can be loaded from different module contexts
# but the module context of `IRShowSandbox` should be consistent

const IRShowSandbox = Module()

Core.eval(IRShowSandbox, Meta.parseall(raw"""
function foo(x, y)
    z = x + y
    if z < 4
        z += 1
    end
    u = (x -> x + z)(x)
    v = Ref{Union{Int, Missing}}(x)[] + y
    return u + v
end
"""; filename=basename(@__FILE__)))
