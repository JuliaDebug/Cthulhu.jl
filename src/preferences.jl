function save_config!(config::CthulhuConfig, force=true)
    # Call Preferences.set_preferences!() without introducing a dependency on Preferences
    id = Base.PkgId(UUID("21216c6a-2e73-6563-6e65-726566657250"), "Preferences")
    mod = get(Base.loaded_modules, id, nothing)
    if mod == nothing
        @warn "Preferences.jl must be loaded in order to save configuration to file."
    else
        set_preferences! = getfield(mod, :set_preferences!)::Function
        set_preferences!(Cthulhu,
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
end

function read_config!(config::CthulhuConfig)
    # Call Preferences.set_preferences!() without introducing a dependency on Preferences
    id = Base.PkgId(UUID("21216c6a-2e73-6563-6e65-726566657250"), "Preferences")
    mod = get(Base.loaded_modules, id, nothing)
    if mod == nothing
        @info "Preferences.jl not loaded. Using default configuration."
    else
        load_preference = getfield(mod, :load_preference)::Function
        config.enable_highlighter = load_preference(Cthulhu, "enable_highlighter", config.enable_highlighter)
        config.highlighter = Cmd(load_preference(Cthulhu, "highlighter", config.highlighter))
        config.asm_syntax = Symbol(load_preference(Cthulhu, "asm_syntax", config.asm_syntax))
        config.dead_code_elimination = load_preference(Cthulhu, "dead_code_elimination", config.dead_code_elimination)
        config.pretty_ast = load_preference(Cthulhu, "pretty_ast", config.pretty_ast)
        config.debuginfo = Symbol(load_preference(Cthulhu, "debuginfo", config.debuginfo))
        config.optimize = load_preference(Cthulhu, "optimize", config.optimize)
        config.iswarn = load_preference(Cthulhu, "iswarn", config.iswarn)
        config.remarks = load_preference(Cthulhu, "remarks", config.remarks)
        config.with_effects = load_preference(Cthulhu, "with_effects", config.with_effects)
        config.inline_cost = load_preference(Cthulhu, "inline_cost", config.inline_cost)
        config.type_annotations = load_preference(Cthulhu, "type_annotations", config.type_annotations)
    end
end
