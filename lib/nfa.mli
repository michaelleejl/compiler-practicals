type nfa 

val empty: nfa 
val epsilon: nfa 
val single: char -> nfa 
val alt : nfa -> nfa -> nfa 
val seq : nfa -> nfa -> nfa 
val kleene: nfa -> nfa  