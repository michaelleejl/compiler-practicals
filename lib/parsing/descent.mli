open Mlot

type token = Mlot_Token.t
type ast = Mlot_Ast.node

exception ParseFail of string

val parse : token list -> ast
