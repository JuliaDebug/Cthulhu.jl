# Cthulhu.jl
*The slow descent into madness*

Cthulhu can help you debug type inference issues by recursively showing the 
`code_typed` output until you find the exact point where inference gave up, 
messed up, or did something unexpected. Using the Cthulhu interface you can
debug type inference problems faster.

```julia
descend(f, tt)
@descend f()
```

Given a function and a tuple-type, interactively explore the output of
`code_typed` by descending into `invoke` and `call` statements. (`invoke`
statements correspond to static dispatch, whereas `call` statements correspond
to dynamic dispatch.) Press enter to select an `invoke` or `call` to descend
into, select ↩  to ascend, and press q or control-c to quit.

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
- `@descend`: Shortcut for `@descend_code_typed`
- `descend`: Shortcut for `descend_code_typed`
