module CthulhuCompilerExt

@static if VERSION ≥ v"1.12.0-DEV.1581"
    using Compiler: Compiler as CC
    using Compiler.IRShow: IRShow
    using Cthulhu: Cthulhu

    function __init__()
        CC.activate!(; reflection=true)
        Cthulhu.CTHULHU_MODULE[] = @__MODULE__
    end

    include("../src/CthulhuBase.jl")
end

end
