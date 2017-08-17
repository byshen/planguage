(* Dan Grossman, CSE341, HW3 Provided Code *)

exception NoAnswer

datatype pattern = WildcardP
		 | VariableP of string
		 | UnitP
		 | ConstantP of int
		 | ConstructorP of string * pattern
		 | TupleP of pattern list

datatype valu = Constant of int
	      | Unit
	      | Constructor of string * valu
	      | Tuple of valu list

fun g f1 f2 p =
  (*g is the function to compute the value of a pattern p*)
  let 
      val r = g f1 f2 
  in
      case p of
	  WildcardP         => f1 ()
	| VariableP x       => f2 x
	| ConstructorP(_,p) => r p
	| TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	| _                 => 0
  end
      
(**** for the challenge problem only ****)

datatype typ = AnythingT
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | DatatypeT of string

(**** you can put all your code here ****)

(* p1 *)
fun only_lowercase xs = List.filter (fn s => Char.isLower(String.sub(s,0))) xs
	      
(* p2 *)
fun longest_string1 xs =
  List.foldl (fn (x,acc) => if String.size x > String.size acc then x else acc) "" xs 
 
(* p3 *)
fun longest_string2 xs =
  List.foldl (fn (x,acc) => if String.size x >= String.size acc then x else acc) "" xs 

(* p4 *)
fun longest_string_helper f xs =
  List.foldl (fn (x,acc) => if f (String.size x, String.size acc) then x else acc) "" xs 

fun longest_string3 xs = longest_string_helper (fn (x,y)=> x>y) xs	       
fun longest_string4 xs = longest_string_helper (fn (x,y)=> x>=y) xs
(* p5 *)
fun longest_lowercase xs =
  let val f = longest_string1 o only_lowercase
  in f xs
  end
(* p6 *)
fun rev_string st =
  let val f = String.implode o rev o String.explode
      val g = String.map Char.toUpper st
  in
      f g
  end
(* p7 *)
fun first_answer f xs =
  case xs of
      [] => raise NoAnswer
    | x::xs' => case f x of
		    NONE => first_answer f xs'
		  | SOME t => t
(* p8 *)
fun all_answers f xs =
  let fun aux (f, xs, acc) =
	  case xs of
	      [] => SOME acc
	    | x::xs' => case f x of
			    NONE => NONE
			  | SOME t => aux(f, xs', acc@t)
  in aux(f, xs, [])
  end
(* p9 *)
val count_wildcards = g (fn ()=>1) (fn p=>0)
val count_wild_and_variable_lengths = g (fn ()=> 1) String.size

fun count_a_var st pat =
  g (fn ()=>0) (fn s=> if s = st then 1 else 0) pat

(* p10 *)
fun check_pattern pat =
  let fun retrieve (pat,acc) =
	case pat of
	    VariableP p       => p::acc
	  | ConstructorP(_,p) => retrieve (p, acc)
	  | TupleP ps         => List.foldl (fn (p,acc) => retrieve(p,acc)) acc ps
	  | _                 => acc
      fun check_strs xs =
	case xs of
	    [] => true
	  | x::xs' => if List.exists (fn p => p=x) xs'
		      then check_strs xs'
		      else false
  in
      check_strs(retrieve(pat, []))
  end
(*p11*)
fun match (va, pat) =
  case (va, pat) of
      (_, WildcardP) => SOME []
    | (Unit, UnitP)  => SOME []
    | (v, VariableP s) => SOME [(s,v)]
    | (Constant c, ConstantP p) => if c=p then SOME [] else NONE
    | (Constructor(s2,v), ConstructorP(s1,p)) => if s1 = s2
						 then match(v,p)
						 else NONE
    | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
			       then all_answers match (ListPair.zip(vs, ps))
			       else NONE
    | (_, _) => NONE
(*p12*)
fun first_match va ps =
  SOME( first_answer (fn (p)=> match(va,p)) ps)
  handle NoAnswer =>NONE
				   
(* challenges *)
(* The first step is to transfer from pattern to the specific type. Note that
 ConstructorP requires the type in (s, s, typ) is smaller than the type defined in ConstructorP.
*)
fun get_typs_from_pattern(p, lst) =
  case p of
      ConstantP _ => IntT
    | UnitP => UnitT
    | TupleP ps => TupleT (List.map(fn x=> get_typs_from_pattern(x, lst)) ps)
    | ConstructorP (str, cp) => (* constructor name the same and type is also
				 the same or AnythingT*)
      let
	  fun find_str x =
	      let val typ_of_cp = get_typs_from_pattern(cp, lst)
	      in 
		  case x of 
		      (s1, s2, t) => s1 = str andalso (typ_of_cp = t orelse typ_of_cp = AnythingT)
	      end
      in
	  case List.find find_str lst of
	      SOME (s1, s2, t) => DatatypeT s2
	    | NONE             => raise NoAnswer
      end
    | _ => AnythingT

(* A very elegant verison to get the most strict common type, better understanding 
of   List.map and the use of ListPair.zip
*)
fun get_lenient (t1, t2) =
  if t1 = t2 then t1 else
  case (t1, t2) of
      (_, AnythingT)    => t1
    | (AnythingT, _)    => t2
    | (TupleT ts1, TupleT ts2)  => if List.length ts1 = List.length ts2
				   then TupleT (List.map get_lenient (ListPair.zip(ts1, ts2)) )
				   else raise NoAnswer
    | (_, _) => raise NoAnswer 
				       
(* 1. Find all types of given patterns, if any of the pattern is NONE, then NONE
   2. Else return the most strict common type.
 *)      
fun typecheck_patterns(lst, pst) =
  let val typs = List.map (fn x =>  get_typs_from_pattern(x, lst))  pst
		      handle NoAnswer => []
  in
      case typs of
	  []         => NONE
	| head::tail => SOME( List.foldl get_lenient head tail)
			    handle NoAnswer => NONE
  end
      

      (* List.foldl f init l*)
      (* List.map f l*)
      (* List.exists f l*)
      (* List.find f l*)
      (* ListPair.zip(a, b) *)
