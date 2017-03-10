(*This is a comment.*)

val x = 1;
(* dynamic environment. x --> 34 *)

val y = 17;

val z = x -  y;

val abs_of_z = if z < 0 then 0 - z else z; (* bool *)
(* dynamic environment : ...*)

val abs_z = abs z;

fun test(arr : int * int * int, brr: int*int*int) =
	if #3 arr = 8
	then true
	else if #3 brr = 8
	then true
	else
	    false
		   
	    
