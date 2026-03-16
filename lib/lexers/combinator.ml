open Base
open Intfs

module LexerCombinator : Lexer = struct
  exception LexFailure

  type match_state = { matched : char list; rest : char list }
  type lex_state = { lexed : token list; rest : char list }
  type 'a outcome = Success of 'a | Failure
  type matcher = char list -> match_state outcome

  let promote m ?(ignore = false) to_token { lexed; rest } =
    match m rest with
    | Failure -> Failure
    | Success { matched; rest } ->
        if ignore then Success { lexed; rest }
        else
          let t = to_token matched in
          Success { lexed = t :: lexed; rest }

  let pattern_match f cs =
    match cs with
    | [] -> Failure
    | c :: cs -> if f c then Success { matched = [ c ]; rest = cs } else Failure

  let alphabetic = pattern_match Char.is_alpha
  let numeric = pattern_match Char.is_digit
  let alphanumeric = pattern_match Char.is_alphanum
  let whitespace_matcher = pattern_match Char.is_whitespace
  let chr x = pattern_match (Char.equal x)
  let match_epsilon s = Success { matched = []; rest = s }

  let match_seq m1 m2 cs =
    match m1 cs with
    | Failure -> Failure
    | Success { matched = matched1; rest } -> (
        match m2 rest with
        | Failure -> Failure
        | Success { matched = matched2; rest } ->
            Success { matched = matched1 @ matched2; rest })

  let match_alt (m1 : matcher) m2 cs =
    match (m1 cs, m2 cs) with
    | Failure, Failure -> Failure
    | Success s, Failure -> Success s
    | Failure, Success s' -> Success s'
    | Success s, Success s' ->
        if List.length s.rest <= List.length s'.rest then Success s
        else Success s'

  let match_kleene m cs =
    let rec match_kleene' m cs =
      match m cs with
      | Failure -> { matched = []; rest = cs }
      | Success { matched; rest } ->
          let { matched = matched'; rest = rest' } = match_kleene' m rest in
          { matched = matched @ matched'; rest = rest' }
    in
    Success (match_kleene' m cs)

  let ( >& ) = match_seq
  let ( >| ) = match_alt
  let ( ~* ) = match_kleene
  let match_plus m = m >& ~*m
  let ( ~+ ) = match_plus
  let match_maybe m = m >| match_epsilon
  let ( ~? ) = match_maybe

  let str s =
    let cs' = String.to_list s in
    List.fold cs' ~init:match_epsilon ~f:(fun acc -> fun x -> acc >& chr x)

  let ident_matcher = alphabetic >& ~*alphanumeric
  let literal_matcher = ~?(str "-") >& ~+numeric
  let empty _ = Failure
  let epsilon s = Success s
  let seq l1 l2 s = match l1 s with Failure -> Failure | Success s' -> l2 s'

  let alt l1 l2 s =
    match (l1 s, l2 s) with
    | Failure, Failure -> Failure
    | Success s, Failure -> Success s
    | Failure, Success s' -> Success s'
    | Success s, Success s' ->
        if List.length s.rest <= List.length s'.rest then Success s
        else Success s'

  let rec kleene l s =
    match l s with Failure -> Success s | Success s' -> kleene l s'

  let ( >>& ) = seq
  let ( >>| ) = alt
  let ( ~~* ) = kleene
  let ident = promote ident_matcher (fun cs -> IDENT (cs |> String.of_list))

  let literal =
    promote literal_matcher (fun cs ->
        NUM (cs |> String.of_list |> Int.of_string))

  let whitespace =
    promote whitespace_matcher (fun _ -> assert false) ~ignore:true

  let keywords =
    [
      ("let", fun _ -> LET);
      ("rec", fun _ -> REC);
      ("in", fun _ -> IN);
      ("fun", fun _ -> FUN);
      ("true", fun _ -> TRUE);
      ("false", fun _ -> FALSE);
    ]

  let operators =
    [
      ("=", fun _ -> EQUALS);
      ("+", fun _ -> PLUS);
      ("->", fun _ -> ARROW);
      ("(", fun _ -> LPARAN);
      (")", fun _ -> RPARAN);
    ]

  let to_lexer xs =
    List.fold xs ~init:empty ~f:(fun acc ->
        fun (s, to_token) -> acc >>| promote (str s) to_token ~ignore:false)

  let lex_one =
    to_lexer keywords >>| to_lexer operators >>| ident >>| literal
    >>| whitespace

  let lex_many = epsilon >>| lex_one >>& ~~*(whitespace >>& lex_one)

  let lex s =
    let cs = String.to_list s in
    match lex_many { lexed = []; rest = cs } with
    | Success { lexed; rest = [] } -> List.rev lexed
    | _ -> raise LexFailure
end
