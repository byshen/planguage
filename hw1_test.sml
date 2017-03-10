(* test for is_older 
is_older((1,1,1988),(1,2,1999));
is_older((1,1,1988),(1,2,1998));
is_older((1,2,1989),(1,2,1988));
is_older((1,2,1989),(1,2,1989));
is_older((29,1,1988),(1,2,1988));
is_older((2,2,1989),(1,2,1988));
 *)

(* test for number_in_month *)

val date_list = [(1,2,131),
		 (1,3,1313),
		 (1,3,131),
		 (2,4,3131),
		 (1,3,131),
		 (3,5,443),
		 (1,6,1331),
		 (1,7,5),
		 (3,9,134)
		];
		
number_in_month(date_list, 3);
number_in_month(date_list, 4);
number_in_month(date_list, 5);

