# Changelog for Cthulhu

## `v3.0.0`

### Improvements

- The way that packages integrate with Cthulhu to customize the behavior of code introspection has been redesigned (see https://github.com/JuliaDebug/Cthulhu.jl/pull/662 and https://github.com/JuliaDebug/Cthulhu.jl/pull/677 for more details).
- A new UI command mapped to `DEL` (backspace, `'\x7f'`) now allows to go back (ascend) with a single key press.

### Breaking changes

- The `AbstractCursor` interface was removed, deleting or changing the signature of most associated methods (`Cthulhu.AbstractCursor`, `Cthulhu.lookup`, `Cthulhu.lookup_constproped`, `Cthulhu.get_ci`, `Cthulhu.update_cursor`, `Cthulhu.navigate`)
- The `Cthulhu.custom_toggles` interface was removed, along with `Cthulhu.CustomToggle`, replaced by `Cthulhu.menu_commands` and `Cthulhu.Command`, respectively.
- `CthulhuConfig` is no longer mutable (but `CthulhuState`, the new state structure holding it, is mutable). `CONFIG` was therefore changed to a global typed but non-const variable, which may be reassigned by users (instead of mutating its fields) with `Cthulhu.CONFIG = my_new_config`. See [`set_config`] and [`set_config!`] to modify an existing configuration, and [`save_config!`] to persist a configuration with Preferences.jl.
- The `annotate_source` configuration option was removed in favor of a `view::Symbol` configuration option, with the `:source|:ast|:typed|:llvm|:native` available views by default (more may be defined by providers).
- The `exception_type` configuration option was renamed to `exception_types` for consistency with `remarks`.
- The `with_effects` configuration option was renamed to `effects` for consistency with `remarks` and `exception_types`.
- The `inline_cost` configuration option was renamed to `inlining_costs`, also for consistency reasons.
- The `interruptexc` configuration option was removed. It used to control whether `q` exited (by throwing an `InterruptException`) or ascended, but now that backspace was added as a shortcut to ascend, we can now unconditionally exit with `q` (which actually matches its action description).
