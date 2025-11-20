"""
```julia
save_config!(config::CthulhuConfig=CONFIG; kwargs...)
```
Save a Cthulhu.jl configuration `config` (by default, `Cthulhu.CONFIG`) to your
`LocalPreferences.toml` file using Preferences.jl. For more information about
`kwargs`, refer to the documentation of `Preferences.set_preferences!`.

The saved preferences will be automatically loaded next time you `using Cthulhu`.

See also: [`set_config`](@ref), [`set_config!`](@ref)

## Examples
```julia
julia> using Cthulhu

julia> Cthulhu.CONFIG.enable_highlighter = true
true

julia> Cthulhu.CONFIG.debuginfo = :none     # Customize some defaults
:none

julia> Cthulhu.save_config!(Cthulhu.CONFIG) # Will be automatically read next time you `using Cthulhu`

julia> Cthulhu.save_config!(Cthulhu.CONFIG; force = true) # to overwrite any existing preferences
```
"""
function save_config!(config::CthulhuConfig=CONFIG; kwargs...)
    set_preferences!(Cthulhu,
        "enable_highlighter" => config.enable_highlighter,
        "highlighter" => config.highlighter.exec,
        "asm_syntax" => String(config.asm_syntax),
        "pretty_ast" => config.pretty_ast,
        "debuginfo" => String(config.debuginfo),
        "optimize" => config.optimize,
        "iswarn" => config.iswarn,
        "remarks" => config.remarks,
        "effects" => config.effects,
        "inlining_costs" => config.inlining_costs,
        "type_annotations" => config.type_annotations,
        "view" => String(config.view),
        "inlay_types_vscode" => config.inlay_types_vscode,
        "diagnostics_vscode" => config.diagnostics_vscode,
        "jump_always" => config.jump_always; kwargs...)
end

function read_config!()
    global CONFIG
    @reset CONFIG.enable_highlighter = load_preference(Cthulhu, "enable_highlighter", CONFIG.enable_highlighter)
    @reset CONFIG.highlighter = Cmd(load_preference(Cthulhu, "highlighter", CONFIG.highlighter))
    @reset CONFIG.asm_syntax = Symbol(load_preference(Cthulhu, "asm_syntax", CONFIG.asm_syntax))
    @reset CONFIG.pretty_ast = load_preference(Cthulhu, "pretty_ast", CONFIG.pretty_ast)
    @reset CONFIG.debuginfo = Symbol(load_preference(Cthulhu, "debuginfo", CONFIG.debuginfo))
    @reset CONFIG.optimize = load_preference(Cthulhu, "optimize", CONFIG.optimize)
    @reset CONFIG.iswarn = load_preference(Cthulhu, "iswarn", CONFIG.iswarn)
    @reset CONFIG.remarks = load_preference(Cthulhu, "remarks", CONFIG.remarks)
    @reset CONFIG.effects = load_preference(Cthulhu, "effects", CONFIG.effects)
    @reset CONFIG.inlining_costs = load_preference(Cthulhu, "inlining_costs", CONFIG.inlining_costs)
    @reset CONFIG.type_annotations = load_preference(Cthulhu, "type_annotations", CONFIG.type_annotations)
    @reset CONFIG.view = Symbol(load_preference(Cthulhu, "view", CONFIG.view))
    @reset CONFIG.inlay_types_vscode = load_preference(Cthulhu, "inlay_types_vscode", CONFIG.inlay_types_vscode)
    @reset CONFIG.diagnostics_vscode = load_preference(Cthulhu, "diagnostics_vscode", CONFIG.diagnostics_vscode)
    @reset CONFIG.jump_always = load_preference(Cthulhu, "jump_always", CONFIG.jump_always)
    return CONFIG
end

"""
    set_config(config::CthulhuConfig = CONFIG; parameters...)
    set_config(config::CthulhuConfig, parameters::NamedTuple)

Create a new `CthulhuConfig` from the parameters provided as keyword arguments,
with all other parameters identical to those of `config`.
"""
function set_config end

set_config(config::CthulhuConfig = CONFIG; parameters...) = set_config(config, NamedTuple(parameters))
set_config(config::CthulhuConfig, parameters::NamedTuple) = setproperties(config, parameters)

"""
    set_config!(; kwargs...)

Create a new `CthulhuConfig` with [`set_config`](@ref), then update the binding `Cthulhu.CONFIG`
to now refer to that object.
"""
function set_config!(; kwargs...)
    global CONFIG = set_config(CONFIG; kwargs...)
end
