# Cthulhu.jl
**The slow descent into madness**

```
explore_code_typed(f, tt)
```

Given a function and a tuple-type, interactively explore the output of
`code_typed` by descending into `invoke` statements. Type enter to select an
`invoke` to descend into, select ↩  to ascend, and press q or control-c to
quit.

# Usage:
```julia
function foo()
    T = rand() > 0.5 ? Int64 : Float64
    sum(rand(T, 100))
end

explore_code_typed(foo, Tuple{})
```
