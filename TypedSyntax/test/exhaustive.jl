using TypedSyntax
using MethodAnalysis
using ProgressMeter
using Test
using Logging
using CodeTracking: CodeTracking

const missingmethods = Set{Method}()
const badmis = Core.MethodInstance[]
const goodmis = Core.MethodInstance[]
@testset "Exhaustive" begin
    @showprogress for mi in methodinstances(Base)
        isa(mi.def, Method) || continue
        m = mi.def::Method
        isdefined(m, :generator) && continue   # code_typed can't handle this
        m ∈ missingmethods && continue         # we tried before and couldn't find the source text
        m.name === :kwcall && mi.specTypes.parameters[3] === Type{Union{}} && continue # skip kwcall definition in base/boot.jl
        cis = Base.code_typed_by_type(mi.specTypes; debuginfo=:source, optimize=false)
        if length(cis) == 1
            src, rt = cis[1]
            # Can CodeTracking handle this?
            ret = with_logger(NullLogger()) do
                CodeTracking.definition(String, m)
            end
            if ret === nothing
                push!(missingmethods, m)
                continue
            end
            try
                tsn, _ = TypedSyntax.tsn_and_mappings(mi, src, rt, ret...; warn=false)
                @test isa(tsn, TypedSyntaxNode)
                push!(goodmis, mi)
            catch
                push!(badmis, mi)
            end
        end
    end

    @test_broken isempty(badmis)
    @test length(badmis) < 0.01 * length(goodmis)
end
