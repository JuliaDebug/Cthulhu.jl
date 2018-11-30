# Cthulhu.jl
*The slow descent into madness*

```julia
descend(f, tt)
@descend f()
```

Given a function and a tuple-type, interactively explore the output of
`code_typed` by descending into `invoke` statements. Type enter to select an
`invoke` to descend into, select ↩  to ascend, and press q or control-c to
quit.

## Usage

```julia
function foo()
    T = rand() > 0.5 ? Int64 : Float64
    sum(rand(T, 100))
end

descend(foo, Tuple{})
@descend foo()
```

## Methods

- `@descend_code_typed`
- `descend_code_typed`
- `@descend_code_warntype`
- `descend_code_warntype`
- `@descend`: Shortcut for `@descend_code_warntype`
- `descend`: Shortcut for `descend_code_warntype`
