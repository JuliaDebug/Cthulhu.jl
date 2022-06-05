
"""
```julia
save_config!(config::CthulhuConfig=CONFIG, force=true)
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
function save_config!(config::CthulhuConfig=CONFIG, force=true)
    cthulhu_id = UUID("f68482b8-f384-11e8-15f7-abe071a5a75f")
    set_preferences!(cthulhu_id,
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
        force=force
    )
end

function read_config!(config::CthulhuConfig)
    cthulhu_id = UUID("f68482b8-f384-11e8-15f7-abe071a5a75f")
    config.enable_highlighter = load_preference(cthulhu_id, "enable_highlighter", config.enable_highlighter)
    config.highlighter = Cmd(load_preference(cthulhu_id, "highlighter", config.highlighter))
    config.asm_syntax = Symbol(load_preference(cthulhu_id, "asm_syntax", config.asm_syntax))
    config.dead_code_elimination = load_preference(cthulhu_id, "dead_code_elimination", config.dead_code_elimination)
    config.pretty_ast = load_preference(cthulhu_id, "pretty_ast", config.pretty_ast)
    config.debuginfo = Symbol(load_preference(cthulhu_id, "debuginfo", config.debuginfo))
    config.optimize = load_preference(cthulhu_id, "optimize", config.optimize)
    config.iswarn = load_preference(cthulhu_id, "iswarn", config.iswarn)
    config.remarks = load_preference(cthulhu_id, "remarks", config.remarks)
    config.with_effects = load_preference(cthulhu_id, "with_effects", config.with_effects)
    config.inline_cost = load_preference(cthulhu_id, "inline_cost", config.inline_cost)
    config.type_annotations = load_preference(cthulhu_id, "type_annotations", config.type_annotations)
end
