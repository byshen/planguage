val date_list = [(1,2,131),
		 (1,3,1313),
		 (1,3,131),
		 (2,4,3131),
		 (1,3,1312),
		 (3,5,443),
		 (1,6,1331),
		 (1,7,5),
		 (3,9,134)
		];

val fun1_test1 = is_older((1,1,1988),(1,2,1999)) = true;
val fun1_test2 = is_older((1,1,1988),(1,2,1998)) = true;
val fun1_test3 = is_older((1,2,1989),(1,2,1988)) = false;
val fun1_test4 = is_older((1,2,1989),(1,2,1989)) = false;
val fun1_test5 = is_older((29,1,1988),(1,2,1988)) = true;
val fun1_test6 = is_older((2,2,1989),(1,2,1988)) = false;

val fun2_test1 = number_in_month(date_list, 3) = 3;
val fun2_test2 = number_in_month(date_list, 4) = 1;
val fun2_test3 = number_in_month(date_list, 5) = 1;

val fun3_test1 = number_in_months(date_list, [3]) = 3;
val fun3_test2 = number_in_months(date_list, [3,5]) = 4;
val fun3_test3 = number_in_months(date_list, [4,5]) = 2;

val fun4_test1 = dates_in_month(date_list, 3) = [(1,3,1313), (1,3,131), (1,3,1312)];
val fun4_test2 = dates_in_month(date_list, 4) = [(2,4,3131)];
val fun4_test3 = dates_in_month(date_list, 5) = [(3,5,443)];


val fun5_test1 = dates_in_months(date_list, [3, 4]) = [(1,3,1313), (1,3,131), (1,3,1312), (2,4,3131)];
val fun5_test2 = dates_in_months(date_list, [4, 5]) = [(2,4,3131), (3,5,443)];
val fun5_test3 = dates_in_months(date_list, [3, 5]) = [(1,3,1313), (1,3,131), (1,3,1312), (3,5,443)];


val fun6_test1 = get_nth(["a", "b", "c", "d"], 1) = "a";
val fun6_test2 = get_nth(["a", "b", "c", "d"], 2) = "b";
val fun6_test3 = get_nth(["a", "b", "c", "d"], 3) = "c";

val fun7_test1 =  date_to_string((1,2,1999)) = "February-1-1999";
val fun7_test2 =  date_to_string((20,8,1995)) = "August-20-1995";
val fun7_test3 =  date_to_string((27,12,1994)) = "December-27-1994";

val fun8_test1 = number_before_reaching_sum(10, [1,2,3,4,5]) = 3;
val fun8_test2 = number_before_reaching_sum(10, [1,1,2,3,3,5]) = 4;
val fun8_test3 = number_before_reaching_sum(10, [1,1,1,1,1,4,2]) = 6;

val fun9_test1 = what_month(1) = 1;
val fun9_test2 = what_month(31) = 1;
val fun9_test3 = what_month(32) = 2;
val fun9_test4 = what_month(365) = 12;

val fun10_test1 = month_range(31,32) = [1,2];
val fun10_test2 = month_range(365,365) = [12];
val fun10_test3 = month_range(2,1) = [];

val fun11_test1 = oldest(date_list) = SOME (1,7,5);
val fun11_test2 = oldest([]) = NONE;

val fun12_test1 = cumulative_sum([1,2,3]) = [1,3,6];
val fun12_test2 = cumulative_sum([1,1,1]) = [1,2,3];
val fun12_test3 = cumulative_sum([1,20,300]) = [1,21,321];

(* challenge 13 is boring *)

val fun14_test1 = reasonable_date((29,2,2000)) = true;
val fun14_test2 = reasonable_date((29,2,1900)) = false;
val fun14_test3 = reasonable_date((29,2,2004)) = true;
val fun14_test4 = reasonable_date((13,3,2017)) = true;
