
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
    @set_preferences!(
        "enable_highlighter" => config.enable_highlighter,
        "highlighter" => config.highlighter.exec,
        "asm_syntax" => String(config.asm_syntax),
        "dead_code_elimination" => config.dead_code_elimination,
        "pretty_ast" => config.pretty_ast,
        "debuginfo" => String(config.debuginfo),
        "optimize" => config.optimize,
        "iswarn" => config.iswarn,
        "remarks" => config.remarks,
        "with_effects" => config.with_effects,
        "inline_cost" => config.inline_cost,
        "type_annotations" => config.type_annotations,
        "annotate_source" => config.annotate_source,
        "hide_inlay_types_vscode" => config.hide_inlay_types_vscode,
        "hide_warn_diagnostics_vscode" => config.hide_warn_diagnostics_vscode,
    )
end

function read_config!(config::CthulhuConfig)
    config.enable_highlighter = @load_preference("enable_highlighter", config.enable_highlighter)
    config.highlighter = Cmd(@load_preference("highlighter", config.highlighter))
    config.asm_syntax = Symbol(@load_preference("asm_syntax", config.asm_syntax))
    config.dead_code_elimination = @load_preference("dead_code_elimination", config.dead_code_elimination)
    config.pretty_ast = @load_preference("pretty_ast", config.pretty_ast)
    config.debuginfo = Symbol(@load_preference("debuginfo", config.debuginfo))
    config.optimize = @load_preference("optimize", config.optimize)
    config.iswarn = @load_preference("iswarn", config.iswarn)
    config.remarks = @load_preference("remarks", config.remarks)
    config.with_effects = @load_preference("with_effects", config.with_effects)
    config.inline_cost = @load_preference("inline_cost", config.inline_cost)
    config.type_annotations = @load_preference("type_annotations", config.type_annotations)
    config.annotate_source = @load_preference("annotate_source", config.annotate_source)
    config.hide_inlay_types_vscode = @load_preference("hide_inlay_types_vscode", config.hide_inlay_types_vscode)
    config.hide_warn_diagnostics_vscode = @load_preference("hide_warn_diagnostics_vscode", config.hide_warn_diagnostics_vscode)
end
