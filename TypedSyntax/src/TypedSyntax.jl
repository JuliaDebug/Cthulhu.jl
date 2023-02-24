module TypedSyntax

using Core: CodeInfo, MethodInstance, SlotNumber, SSAValue
using Core.Compiler: TypedSlot
using JuliaSyntax: JuliaSyntax, TreeNode, AbstractSyntaxData, SyntaxData, SyntaxNode, GreenNode, SyntaxHead, SourceFile,
                   head, kind, child, children, haschildren, untokenize, first_byte, last_byte, source_line, source_location,
                   sourcetext, @K_str, @KSet_str, is_infix_op_call, is_prefix_op_call, is_prec_assignment
using Base.Meta: isexpr
using CodeTracking

export TypedSyntaxNode

include("node.jl")
include("show.jl")

end
