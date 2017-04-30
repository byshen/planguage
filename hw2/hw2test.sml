(* Dan Grossman, CSE341, HW2 Provided Tests *)

(* These are just two tests for problem 2; you will want more.

   Naturally these tests and your tests will use bindings defined 
   in your solution, in particular the officiate function, 
   so they will not type-check if officiate is not defined.
 *)
val p1_test1 = remove_option("a", ["d", "b", "c"]) = NONE;
val p1_test2 = remove_option("a", ["a", "b", "c"]) = SOME(["b", "c"]);

val p2_test1 = all_substitutions1([["a", "b"], ["a", "c"], ["a", "d"]], "a")
	       =["b", "c", "d"];				 

val p3_test1 = all_substitutions2([["a", "b"], ["a", "c"], ["a", "d"]], "a")
	       =["b", "c", "d"];
val p4_test1 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"});(*
	       =
	       [{first="Fred", last="Smith", middle="W"},
		{first="Fredrick", last="Smith", middle="W"},
		{first="Freddie", last="Smith", middle="W"},
		{first="F", last="Smith", middle="W"}];*)


val test5 = card_color (Num 2, Clubs) = Black

val test6 = card_value (Num 2, Clubs) = 2

val test7 = remove_card ([(Ace, Hearts)], (Ace, Hearts), IllegalMove) = []

val test8 = all_same_color [(Ace, Hearts), (Ace, Hearts)] = true

val test9 = sum_cards [ (Num 2, Clubs),  (Num 2, Clubs)] = 4

val test10 = score ([(Num 2, Hearts),(Num 4, Clubs)],10) = 4


fun provided_test1 () = (* correct behavior: raise IllegalMove *)
    let val cards = [(Jack,Clubs),(Num(8),Spades)]
	val moves = [Draw,Discard(Jack,Hearts)]
    in
	officiate(cards,moves,42)
    end

fun provided_test2 () = (* correct behavior: return 5 *)
    let val cards = [(Ace,Clubs),(Ace,Spades),(Ace,Clubs),(Ace,Spades)]
	val moves = [Draw,Draw,Draw,Draw,Draw]
    in
 	officiate(cards,moves,42)
    end

val v2 = provided_test2() = 5;
