open Mlot
type token = Mlot_Token.t
type ast = Mlot_Ast.node

exception ParseFail of string

module Recogniser: sig 
  val recognise : token list -> bool 
end 

module Parser: sig 
  val parse : token list -> ast 
end 