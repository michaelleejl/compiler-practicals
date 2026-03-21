module StateSet : Set.S with type elt = Nfa.StateSet.elt
module StateMap : Map.S with type key = int
module CharSet : Set.S with type elt = Nfa.CharSet.elt
module CharMap : Map.S with type key = char

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

val determinise : Nfa.t -> t
val accept : t -> string -> bool
