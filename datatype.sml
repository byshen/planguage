datatype mytype = Pizza
		| Str of string
		| int_list of int*int

val a = Pizza
val b = Str("str")
val c = int_list(1,2)

(* null isSome
hd tl valOf
 *)

(*Pattern matching*)
		
fun f (x : mytype) =
  case x of
      Pizza => 0 (*Pattern => value*)
    | Str s  => 1
    | int_list(i1, i2) =>  i1 + 12

