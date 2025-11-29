module TypedSyntax

using Core: CodeInfo, MethodInstance, SlotNumber, SSAValue
using JuliaSyntax: JuliaSyntax, AbstractSyntaxData, SyntaxData, SyntaxNode, GreenNode, AbstractSyntaxNode, SyntaxHead, SourceFile,
                   head, kind, children, is_leaf, untokenize, first_byte, last_byte, source_line, source_location,
                   sourcetext, @K_str, @KSet_str, is_infix_op_call, is_prefix_op_call, is_prec_assignment, is_operator, is_literal
using Base.Meta: isexpr
using CodeTracking: CodeTracking

export TypedSyntaxNode

include("node.jl")
include("vscode.jl")
include("show.jl")

end
