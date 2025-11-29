module CthulhuCompilerExt

using Compiler: Compiler as CC
using Cthulhu: Cthulhu

@static if CC.AbstractInterpreter !== Cthulhu.CC.AbstractInterpreter
    using Compiler.IRShow: IRShow

    using Accessors
    using CodeTracking: CodeTracking
    using InteractiveUtils
    using UUIDs
    using REPL: REPL, AbstractTerminal
    using JuliaSyntax
    using JuliaSyntax: SyntaxNode, AbstractSyntaxNode, child, children
    using TypedSyntax
    using WidthLimitedIO

    using Core.IR

    include("../src/CthulhuCompiler.jl")

    function __init__()
        Cthulhu.CompilerExt = @__MODULE__
    end
else
    function __init__()
        Cthulhu.CompilerExt = nothing
    end
end

end
