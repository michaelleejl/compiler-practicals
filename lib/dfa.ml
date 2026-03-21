module StateSet = Nfa.StateSet
module StateMap = Map.Make (Int)
module CharSet = Nfa.CharSet
module CharMap = Map.Make (Char)

type state = StateSet.elt
type state_set = StateSet.t
type char_set = CharSet.t
type transition = state CharMap.t

type t = {
  initial : state;
  finals : state_set;
  next : state -> transition;
  alphabet : char_set;
}

let find_next_state next q c = CharMap.find c (next q)

let add_transition (source, c, target) transitions =
  match StateMap.find source transitions with
  | exception Not_found ->
      StateMap.add source (CharMap.singleton c target) transitions
  | cm -> StateMap.add source (CharMap.add c target cm) transitions

let determinise n =
  let nfa_initial = Nfa.initialise n in
  let module M = Map.Make (Nfa.StateSet) in
  let gen_state =
    let next_state = ref 0 in
    fun () ->
      let s = !next_state in
      next_state := s + 1;
      s
  in
  let rec build nfa_state (mapping, transitions, finals) =
    match M.find nfa_state mapping with
    | dfa_state -> (dfa_state, mapping, transitions, finals)
    | exception Not_found ->
        let dfa_state = gen_state () in
        let mapping = M.add nfa_state dfa_state mapping in
        let finals =
          if Nfa.contains_final n nfa_state then StateSet.add dfa_state finals
          else finals
        in
        let find_next_state = Nfa.step n nfa_state in
        let builder c (m, t, f) =
          let next_state = find_next_state c in
          let dfa_next_state, m', t', f' = build next_state (m, t, f) in
          let t'' = add_transition (dfa_state, c, dfa_next_state) t' in
          (m', t'', f')
        in
        let mapping', transitions', finals' =
          Nfa.CharSet.fold builder n.alphabet (mapping, transitions, finals)
        in
        (dfa_state, mapping', transitions', finals')
  in
  let initial, _, transitions, finals =
    build nfa_initial (M.empty, StateMap.empty, StateSet.empty)
  in
  let next s =
    try StateMap.find s transitions with Not_found -> CharMap.empty
  in
  let alphabet = n.alphabet in
  { initial; finals; next; alphabet }

let accept d s =
  let cs = Base.String.to_list s in
  let next_state = find_next_state d.next in
  let final = List.fold_left (fun q -> fun c -> next_state q c) d.initial cs in
  StateSet.mem final d.finals
