###
# Reflection tooling
##

# using Base.Meta
# using .Compiler: widenconst, argextype, Const

# if VERSION >= v"1.1.0-DEV.157"
#     const is_return_type = Core.Compiler.is_return_type
# else
#     is_return_type(f) = f === Core.Compiler.return_type
# end

# using Requires
# transform(::Val, callsite) = callsite
# @init @require CUDAnative="be33ccc6-a3ff-5ff2-a52e-74243cff1e17" begin
#     using .CUDAnative
#     function transform(::Val{:CuFunction}, callsite, callexpr, CI, mi, slottypes; params=nothing, kwargs...)
#         sptypes = sptypes_from_meth_instance(mi)
#         tt = argextype(callexpr.args[4], CI, sptypes, slottypes)
#         ft = argextype(callexpr.args[3], CI, sptypes, slottypes)
#         isa(tt, Const) || return callsite
#         return Callsite(callsite.id, CuCallInfo(callinfo(Tuple{widenconst(ft), tt.val.parameters...}, Nothing, params=params)))
#     end
# end

    # function process_return_type(id, c, rt)
    #     callinfo = nothing
    #     is_call = isexpr(c, :call)
    #     arg_base = is_call ? 0 : 1
    #     length(c.args) == (arg_base + 3) || return nothing
    #     ft = argextype(c.args[arg_base + 2], CI, sptypes, slottypes)
    #     argTs = argextype(c.args[arg_base + 3], CI, sptypes, slottypes)
    #     if isa(argTs, Const)
    #         sig = Tuple{widenconst(ft), argTs.val.parameters...}
    #         mi = first_method_instance(sig)
    #         if mi !== nothing
    #             callinfo = MICallInfo(mi, rt.val)
    #         else
    #             callinfo = FailedCallInfo(sig, rt)
    #         end
    #     else
    #         callinfo = FailedCallInfo((ft, argTs), rt)
    #     end
    #     return Callsite(id, ReturnTypeCallInfo(callinfo))
    # end


    # for (id, c) in enumerate(CI.code)
    #     if c isa Expr
    #         callsite = nothing
    #         if c.head === :(=)
    #             c = c.args[2]
    #             (c isa Expr) || continue
    #         end
    #         if c.head === :invoke
    #             rt = CI.ssavaluetypes[id]
    #             at = argextype(c.args[2], CI, sptypes, slottypes)
    #             if isa(at, Const) && is_return_type(at.val)
    #                 callsite = process_return_type(id, c, rt)
    #             else
    #                 callsite = Callsite(id, MICallInfo(c.args[1], rt))
    #             end
    #             mi = get_mi(callsite)
    #             if nameof(mi.def.module) == :CUDAnative && mi.def.name == :cufunction
    #                 callsite = transform(Val(:CuFunction), callsite, c, CI, mi, slottypes; params=params, kwargs...)
    #             end
    #         elseif c.head === :call
    #             rt = CI.ssavaluetypes[id]
    #             types = map(arg -> widenconst(argextype(arg, CI, sptypes, slottypes)), c.args)

    #             # Look through _apply
    #             ok = true
    #             while types[1] === typeof(Core._apply)
    #                 new_types = Any[types[2]]
    #                 for t in types[3:end]
    #                     if !(t <: Tuple) || t isa Union
    #                         ok = false
    #                         break
    #                     end
    #                     append!(new_types, t.parameters)
    #                 end
    #                 ok || break
    #                 types = new_types
    #             end
    #             ok || continue

    #             # Filter out builtin functions and intrinsic function
    #             if types[1] <: Core.Builtin || types[1] <: Core.IntrinsicFunction
    #                 continue
    #             end

    #             if isdefined(types[1], :instance) && is_return_type(types[1].instance)
    #                 callsite = process_return_type(id, c, rt)
    #             else
    #                 if types[1] isa Union
    #                     # Union{typeof(sin), typeof(cos)}
    #                     fts = Any[]
    #                     function thatcher(u)
    #                         if u isa Union
    #                             thatcher(u.a)
    #                             thatcher(u.b)
    #                         else
    #                             push!(fts, u)
    #                         end
    #                     end
    #                     sigs = map(ft-> [ft, types[2:end]], fts)
    #                     cis = map(types -> callinfo(Tuple{types...}, rt, params=params), sigs)
    #                     callsite = Callsite(id, MultiCallInfo(Tuple{types...}, rt, cis))
    #                 else
    #                     callsite = Callsite(id, callinfo(Tuple{types...}, rt, params=params))
    #                 end
    #             end
    #         else c.head === :foreigncall
    #             # special handling of jl_new_task
    #             length(c.args) > 0 || continue
    #             if c.args[1] isa QuoteNode
    #                 cfunc = c.args[1].value
    #                 if cfunc === :jl_new_task
    #                     func = c.args[7]
    #                     ftype = widenconst(argextype(func, CI, sptypes, slottypes))
    #                     callsite = Callsite(id, TaskCallInfo(callinfo(ftype, nothing, params=params)))
    #                 end
    #             end
    #         end

    #         if callsite !== nothing
    #             push!(callsites, callsite)
    #         end
    #     end
    # end
    # return callsites
