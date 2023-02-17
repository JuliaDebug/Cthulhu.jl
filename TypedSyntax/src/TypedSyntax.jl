module TypedSyntax

using Core: CodeInfo, MethodInstance
using JuliaSyntax: JuliaSyntax, TreeNode, AbstractSyntaxData, SyntaxNode, GreenNode, SyntaxHead, SourceFile,
                   head, kind, children, haschildren, untokenize, first_byte, last_byte, source_line, source_location,
                   @K_str, is_infix_op_call, is_prefix_op_call
using Base.Meta: isexpr
using CodeTracking

export TypedSyntaxNode

include("node.jl")
include("show.jl")

end
