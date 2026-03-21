module C = Set.Make(Char)

type 'c rgx = Empty | Epsilon 
          | Char of 'c 
          | Alt of 'c rgx * 'c rgx 
          | Seq of 'c rgx * 'c rgx 
          | Kleene of 'c rgx 

let rec compile r = match r with 
  | Empty -> Nfa.empty
  | Epsilon -> Nfa.epsilon
  | Char cs -> Nfa.one_of (C.to_list cs) 
  | Alt(r1, r2) -> Nfa.alt (compile r1) (compile r2)
  | Seq(r1, r2) -> Nfa.seq (compile r1) (compile r2)
  | Kleene r -> Nfa.kleene (compile r) 

type t = (C.t) rgx 

let empty = Empty 
let epsilon = Epsilon 
let chr c = Char (C.singleton c) 
let alt r1 r2 = match r1, r2 with 
  | Char c1, Char c2 -> Char (C.union c1 c2)
  | r1, r2 -> Alt(r1, r2)
let seq r1 r2 = Seq(r1, r2)
let kleene r = Kleene r 
let plus r = Seq(r, Kleene(r))
let opt r = Alt(Epsilon, r)

let range_ l h =
  let rec loop i h acc = 
    if i = h 
      then C.add (Char.chr i) acc 
      else loop (i+1) h (C.add (Char.chr i) acc) 
  in loop l h C.empty 

let range l h = Char (range_ l h)

let any = Char (range_ 0 255)

module Parse = struct 

  exception Failure 

  type t = Empty 
         | Epsilon 
         | Char of char 
         | Alt of t * t 
         | Seq of t * t 
         | Kleene of t 
         | Plus of t 
         | Opt of t 
         | Any

  let rec parse_atom s = 
    match s with 
      | '('::rest -> begin match parse_alt rest with 
          | r, ')'::rest' -> Some(r, rest')
          | r, rest' -> None
        end 
      | []
      | (')'|'|'|'*'|'+'|'?')::_ -> None 
      | '.'::cs -> Some(Any, cs)
      | c::cs -> Some(Char c, cs)

  and parse_suffixed s = 
    match parse_atom s with 
      | None -> None 
      | Some(r, '*'::rest) -> Some(Kleene(r), rest)
      | Some(r, '?'::rest) -> Some(Opt(r), rest)
      | Some(r, '+'::rest) -> Some(Plus(r), rest)
      | Some(r, rest) -> Some(r, rest)

  and parse_seq s = 
    match parse_suffixed s with 
      | None -> (Epsilon, s)
      | Some(r, rest) -> let r', rest' = parse_seq rest in Seq(r, r'), rest'

  and parse_alt s = 
    match parse_seq s with 
      | (r, '|'::rest) -> let (r', rest') = parse_alt rest in Alt(r, r'), rest'
      | (r, rest) -> r, rest 

  let parse s = 
    let cs = Base.String.to_list s in 
    match parse_alt cs with 
      | (r, []) -> r 
      | (r, _) -> raise Failure
  
  let rec interpret = function 
    | Empty -> empty
    | Epsilon -> epsilon
    | Char c -> chr c
    | Alt (r1, r2) -> alt (interpret r1) (interpret r2)
    | Seq (r1, r2) -> seq (interpret r1) (interpret r2)
    | Kleene r -> kleene (interpret r)
    | Plus r -> plus (interpret r)
    | Opt r -> opt (interpret r)
    | Any -> any 

end 

let parse s = Parse.(interpret (parse s))
