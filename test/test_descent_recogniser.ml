open Printf
open Lexparse.Mlot

open Lexparse.Parsing.Descent.Recogniser

let print_token x = printf "%s ; " (Mlot_Token.to_str x)

let%expect_test _ =
  printf "%b" (recognise [FUN; IDENT "x"; ARROW; IDENT "x"; PLUS; NUM 0]);
  [%expect {| true |}]