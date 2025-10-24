Base.@kwdef struct CthulhuConfig
    enable_highlighter::Bool = false
    highlighter::Cmd = `pygmentize -l`
    asm_syntax::Symbol = :att
    pretty_ast::Bool = false
    debuginfo::Symbol = :compact
    optimize::Bool = true
    iswarn::Bool = false
    hide_type_stable::Bool = false
    remarks::Bool = false
    effects::Bool = false
    exception_types::Bool = false
    inlining_costs::Bool = false
    type_annotations::Bool = true
    inlay_types_vscode::Bool = true
    diagnostics_vscode::Bool = true
    jump_always::Bool = false
    view::Symbol = :source
    menu_options::NamedTuple = ()
    function CthulhuConfig(enable_highlighter, highlighter, asm_syntax, pretty_ast, debuginfo, optimize, iswarn, hide_type_stable, remarks, effects, exception_types, inlining_costs, type_annotations, inlay_types_vscode, diagnostics_vscode, jump_always, view, menu_options)
        diagnostics_vscode &= iswarn # if warnings are off, then no diagnostics are shown
        diagnostics_vscode &= TypedSyntax.diagnostics_available_vscode()
        inlay_types_vscode &= TypedSyntax.inlay_hints_available_vscode()
        optimize &= view !== :source
        return new(enable_highlighter, highlighter, asm_syntax, pretty_ast, debuginfo, optimize, iswarn, hide_type_stable, remarks, effects, exception_types, inlining_costs, type_annotations, inlay_types_vscode, diagnostics_vscode, jump_always, view, menu_options)
    end
end

"""
    Cthulhu.CONFIG

# Options
- `enable_highlighter::Bool`: Use command line `highlighter` to syntax highlight
  Julia, LLVM and native code.
- `highlighter::Cmd`: A command line program that receives "julia" as an argument and julia
   code as stdin. Defaults to `$(CthulhuConfig().highlighter)`.
- `asm_syntax::Symbol`: Set the syntax of assembly code being used.
  Defaults to `$(CthulhuConfig().asm_syntax)`.
- `pretty_ast::Bool`: Use a pretty printer for the ast dump. Defaults to `false`.
- `debuginfo::Symbol`: Initial state of "debuginfo" toggle. Defaults to `:compact`.
  Options:. `:none`, `:compact`, `:source`
- `optimize::Bool`: Initial state of "optimize" toggle. Defaults to `true`.
- `hide_type_stable::Bool`: Initial state of "hide_type_stable" toggle. Defaults to `false`.
- `iswarn::Bool`: Initial state of "warn" toggle. Defaults to `false`.
- `remarks::Bool` Initial state of "remarks" toggle. Defaults to `false`.
- `effects::Bool` Intial state of "effects" toggle. Defaults to `false`.
- `exception_types::Bool` `Intial state of "exception types" toggle. Defaults to `false`.
- `inlining_costs::Bool` Initial state of "inlining costs" toggle. Defaults to `false`.
- `type_annotations::Bool` Initial state of "type annnotations" toggle. Defaults to `true`.
- `view::Symbol` Initial state of the view. Defaults to `:source`. Can be either of `:source`, `:ast`, `:typed`, `:llvm` and `:native`. Non-default `AbstractProvider`s may further customize available views.
- `inlay_types_vscode::Bool` Initial state of "vscode: inlay types" toggle. Defaults to `true`
- `diagnostics_vscode::Bool` Initial state of "Vscode: diagnostics" toggle. Defaults to `true`
- `jump_always::Bool` Initial state of "jump to source always" toggle. Defaults to `false`.

Other keyword arguments are passed to [`Cthulhu.CthulhuMenu`](@ref) and/or
[`REPL.TerminalMenus`](https://docs.julialang.org/en/v1/stdlib/REPL/#Customization-/-Configuration).
"""
global CONFIG::CthulhuConfig = CthulhuConfig()
