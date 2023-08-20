# exact location in file matters
function fib(n)
    if n <= 1 return 1 end
    return fib(n - 1) + fib(n - 2)
end

# exact location in file matters
function fVSCode(x)
    z = x + 1
    y = 2 * z
    return y + (x > 0 ? -1 : 1.0)
end

# exact location in file matters
function fibcall(n)
    fib(Int64(n))
    fib(n)
end