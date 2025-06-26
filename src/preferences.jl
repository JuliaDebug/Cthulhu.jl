"""
```julia
save_config!(config::CthulhuConfig=CONFIG)
```
Save a Cthulhu.jl configuration `config` (by default, `Cthulhu.CONFIG`) to your
`LocalPreferences.toml` file using Preferences.jl.

The saved preferences will be automatically loaded next time you `using Cthulhu`

## Examples
```julia
julia> using Cthulhu

julia> Cthulhu.CONFIG.enable_highlighter = true
true

julia> Cthulhu.CONFIG.debuginfo = :none     # Customize some defaults
:none

julia> Cthulhu.save_config!(Cthulhu.CONFIG) # Will be automatically read next time you `using Cthulhu`
```
"""
function save_config!(config::CthulhuConfig=CONFIG)
    set_preferences!(Cthulhu,
        "enable_highlighter" => config.enable_highlighter,
        "highlighter" => config.highlighter.exec,
        "asm_syntax" => String(config.asm_syntax),
        "pretty_ast" => config.pretty_ast,
        "debuginfo" => String(config.debuginfo),
        "optimize" => config.optimize,
        "iswarn" => config.iswarn,
        "remarks" => config.remarks,
        "with_effects" => config.with_effects,
        "inline_cost" => config.inline_cost,
        "type_annotations" => config.type_annotations,
        "annotate_source" => config.annotate_source,
        "inlay_types_vscode" => config.inlay_types_vscode,
        "diagnostics_vscode" => config.diagnostics_vscode,
        "jump_always" => config.jump_always)
end

function read_config!(config::CthulhuConfig)
    config.enable_highlighter = load_preference(Cthulhu, "enable_highlighter", config.enable_highlighter)
    config.highlighter = Cmd(load_preference(Cthulhu, "highlighter", config.highlighter))
    config.asm_syntax = Symbol(load_preference(Cthulhu, "asm_syntax", config.asm_syntax))
    config.pretty_ast = load_preference(Cthulhu, "pretty_ast", config.pretty_ast)
    config.debuginfo = Symbol(load_preference(Cthulhu, "debuginfo", config.debuginfo))
    config.optimize = load_preference(Cthulhu, "optimize", config.optimize)
    config.iswarn = load_preference(Cthulhu, "iswarn", config.iswarn)
    config.remarks = load_preference(Cthulhu, "remarks", config.remarks)
    config.with_effects = load_preference(Cthulhu, "with_effects", config.with_effects)
    config.inline_cost = load_preference(Cthulhu, "inline_cost", config.inline_cost)
    config.type_annotations = load_preference(Cthulhu, "type_annotations", config.type_annotations)
    config.annotate_source = load_preference(Cthulhu, "annotate_source", config.annotate_source)
    config.inlay_types_vscode = load_preference(Cthulhu, "inlay_types_vscode", config.inlay_types_vscode)
    config.diagnostics_vscode = load_preference(Cthulhu, "diagnostics_vscode", config.diagnostics_vscode)
    config.jump_always = load_preference(Cthulhu, "jump_always", config.jump_always)
end
