fun pow(x: int, y: int) =
  if y = 0
  then 1
  else x * pow(x, y - 1) (* int  * int -> int *)

fun cube(x: int) =
  pow(x, 3)

val t = cube 4
val f = pow(2,2);
(*
 Java with examples: classes with fields, arrays
*)


	   
