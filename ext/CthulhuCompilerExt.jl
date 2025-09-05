module CthulhuCompilerExt

using Compiler: Compiler as CC
using Compiler.IRShow: IRShow
using Cthulhu: Cthulhu

function __init__()
    Cthulhu.CTHULHU_MODULE[] = @__MODULE__
    read_config!()
end

include("../src/CthulhuBase.jl")

end
