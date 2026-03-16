open Compilers.Lexers.Combinator.LexerCombinator
open Compilers.Intfs
open Printf

let print_token = fun x -> printf "%s ; " (token_to_string x)

let%expect_test _ =
  List.iter print_token (lex "fun");
  [%expect {| FUN ; |}]

let%expect_test _ =
  List.iter print_token (lex "fun ->");
  [%expect {| FUN ; ARROW ; |}]

let%expect_test _ =
  List.iter print_token (lex "fun x -> 2");
  [%expect {| FUN ; IDENT x ; ARROW ; NUM 2 ; |}]

let%expect_test _ =
  List.iter print_token (lex "fun x2 -> 2");
  [%expect {| FUN ; IDENT x2 ; ARROW ; NUM 2 ; |}]
