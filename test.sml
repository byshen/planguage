val cbs : (int -> unit) list ref = ref []


fun onEvent i =
  let fun loop fs =
	case fs of
	    [] => ()
	  | f::fs' => (f i; loop fs')
  in loop (!cbs) end

fun onKeyEvent f =
  cbs := f::(!cbs)

val timePressed = ref 0
val _ = onKeyEvent(fn _ => timePressed := !timePressed + 1)

fun printIfPressed i =
  onKeyEvent (fn j =>
		 if i = j
		 then print ("pressed" ^ Int.toString i ^ "\n")
		 else ()
	     )
