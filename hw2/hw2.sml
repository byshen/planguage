(* Dan Grossman, CSE341, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* 1a *)
fun remove_option(s : string, ss: string list) =
  let fun remove(str:string, str_list: string list) = 
	case str_list of
	    [] => []
	  | x :: xs => if same_string(str,x) then xs
		       else x :: remove(str, xs)
      val res = remove(s, ss)
  in
      if res = ss
      then NONE
      else SOME(res)
  end
(* 1a sample from solution
fun remove_option (s1, []) = NONE
  | remove_option (s1, s2::ss) =
    if same_string(s1, s2)
    then NONE
    else
	case remove_option(s1, ss) of
	    NONE => NONE
	  | SOME ss' => SOME (s2::ss')
 *)
(* 1b *)
fun all_substitutions1 ([], s1) =[]
  | all_substitutions1 (s::ss, s1) =
    case remove_option(s1, s) of
	NONE => all_substitutions1(ss, s1)
      | SOME s' => s'@ all_substitutions1(ss, s1)

(* 1c *)
fun all_substitutions2 (ss, s1) = 
  let fun aux(ss, s1, acc) =
	case ss of
	    [] => acc
	  | s::ss' => case remove_option(s1, s) of
			  NONE => aux(ss', s1, acc)
			| SOME s' => aux(ss', s1, acc@s')
  in
      aux(ss, s1, [])
  end
(* 1f *)
fun similar_names (ss: string list list, full_name:{first:string, middle:string, last:string}) =
  let val subs = all_substitutions2(ss, #first full_name)
      val {first = x, middle = y, last = z} = full_name
      fun get_subs(subs: string list, z: string, y: string) =
	case subs of
	    [] => []
	  | sub::subs' => {first = sub, last = z, middle = y} :: get_subs(subs', z, y) 
  in
      {first = x, last = z, middle = y}::get_subs(subs, z, y)
  end  


      
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = rank * suit

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* 2a *)
fun card_color card =
  let val (_, s) = card (*note that for the combined types or records, use val to retrieve the value*)
  in
      case s of
	  Diamonds => Red
	| Hearts => Red
	| _  => Black
      
      
  end
(* 2b *)
fun card_value card =
  let val (r, _) = card
  in
      case r of
	  Ace => 11
	| Num i => i
	| _ => 10
  end
(* 2c *) 
fun remove_card (cs, c, e) =
  let fun remove(cs, c) =
	case cs of
	    [] => []
	  | c'::cs' => if c = c' then cs'
		       else c'::remove(cs', c)
      val res = remove(cs, c)
  in
      if res = cs then raise e
      else res
  end
(* 2d *)
fun all_same_color cs =
  case cs of
      [] => true
    | _::[] => true
    | head::(mid::rest) => (card_color(head) = card_color(mid)) andalso all_same_color(mid::rest)
      
(* 2e *)
fun sum_cards cs =
  let fun aux (cs, acc) =
	case cs of
	    [] => acc
	  | c::cs' => aux(cs', card_value c + acc)
  in
      aux(cs, 0)
  end
      
(* 2f *)
fun score (cs, goal) =
  let val sum = sum_cards cs;
      val pre_score = if sum > goal then 5*(sum - goal)
		      else goal - sum
   in
       if all_same_color(cs) then pre_score div 2
       else pre_score
  end
      
(* 2e *)
fun officiate (card_list, mvs, goal) =
  let fun sub_officiate(card_list, mvs, goal, held_cs) = 
      case mvs of
	  [] => score(held_cs, goal)
	| mv::mvs' =>
	  case mv of
	      Discard c =>
	       let
		   val new_held_cs = remove_card(held_cs, c, IllegalMove)
	       in
		   sub_officiate(card_list, mvs', goal, new_held_cs)
	       end
	    | Draw =>
	      case card_list of
		  [] =>	score(held_cs, goal)
		| c::cs' => let val n_held_cs = c::held_cs
			    in
				if sum_cards n_held_cs > goal
				then score(n_held_cs, goal)
				else sub_officiate(cs', mvs', goal, n_held_cs)
			    end
  in
      sub_officiate(card_list, mvs, goal, [])
  end
      
				    
