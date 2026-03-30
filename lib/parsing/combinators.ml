open Mlot
open Mlot_Token
open Mlot_Ast

type token = Mlot_Token.t
type ast = Mlot_Ast.node

open Intfs

exception ParseFail of string

module Descent
    (Lang : Language.S)
    (Grammar : BNF.S with type token = Lang.token and type ast = Lang.ast) =
struct
  open Grammar

  type token = Lang.token
  type ast = Lang.ast

  let alt p1 p2 toks =
    match p1 toks with toks' -> toks' | exception ParseFail _ -> p2 toks

  let empty ?(msg = "empty") _ = raise (ParseFail msg)

  type table_t = (token list -> data * token list) NontermHashtbl.t

  let table : table_t = NontermHashtbl.create 10

  let init key _ =
    let key_str = string_of_nonterminal key in
    NontermHashtbl.add table key (empty ~msg:key_str)
  ;;

  (* initialise the parse table *)
  NontermHashtbl.iter init grammar;;

  let rec patterns_to_params patterns toks =
    match patterns with
    | [] -> ([], toks)
    | N n :: patterns ->
        let data, toks' = (NontermHashtbl.find table n) toks in
        let param = inject data in
        let params, toks'' = patterns_to_params patterns toks' in
        (param :: params, toks'')
    | T t :: patterns -> (
        try
          let param, toks' = terminal_to_param t toks in
          let params, toks'' = patterns_to_params patterns toks' in
          (param :: params, toks'')
        with Grammar.Fail -> raise (ParseFail "terminal"))

  let lift (patterns, params_to_data) toks =
    let params, toks' = patterns_to_params patterns toks in
    (params_to_data params, toks')

  let populate key pss =
    let f prod acc =
      let p = lift prod in
      alt p acc
    in
    let key_str = string_of_nonterminal key in
    let parse = List.fold_right f pss (empty ~msg:key_str) in
    NontermHashtbl.replace table key parse
  ;;

  (* populate the hash table *)
  NontermHashtbl.iter populate grammar;;

  let parse ts =
    match NontermHashtbl.find table start ts with
    | e, [] -> unwrap e
    | e, _ -> raise (ParseFail "bad parse")
end
