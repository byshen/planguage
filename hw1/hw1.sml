fun is_older(date1 : int * int * int, date2 : int * int * int) =
  if #3 date1 < #3 date2
  then true
  else
      if #3 date1 = #3 date2
      then
	  if #2 date1 < #2 date2
	  then
	      true
	  else
	      if #2 date1 = #2 date2
	      then
		  if #1 date1 < #1 date2
		  then
		      true
		  else
		      false
	      else
		  false
      else
	  false
	      
	      
fun number_in_month(date_list: (int*int*int) list, month: int) =
  if null date_list
  then 0
  else
      let
	  val num_flag = if #2 (hd date_list) = month then 1 else 0
      in
	  num_flag + number_in_month(tl date_list, month)
      end		

	  
fun number_in_months(date_list: (int*int*int) list, months: int list) =
  if null months
  then 0
  else
      number_in_month(date_list, hd months) + number_in_months(date_list, tl months)


fun dates_in_month(date_list: (int*int*int) list, month: int) =
  if null date_list
  then []
  else
      if #2 (hd date_list) = month
      then
	  hd date_list :: dates_in_month(tl date_list, month)
      else
	  dates_in_month(tl date_list, month)

fun dates_in_months(date_list: (int*int*int) list, months: int list) =
  if null months
  then []
  else
      dates_in_month(date_list, hd months) @ dates_in_months(date_list, tl months)

fun get_nth(strs : string list, n : int) =
  if n = 1
  then hd strs
  else
      get_nth(tl strs, n - 1)
							  
fun date_to_string(date : int*int*int) =
  let val strs = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
      get_nth(strs, #2 date) ^ "-" ^ Int.toString(#1 date) ^ "-" ^ Int.toString(#3 date)
  end
      
fun number_before_reaching_sum(sum_target : int, num_list : int list) =
  let
      (*
      fun n_sum(num_list : int list, n : int) =
	if n = 0
	then
	    0
	else
	    hd num_list + n_sum(tl num_list, n - 1)
      *)
      fun judge(sum_target : int, num_list : int list, n : int, sum : int) =
	let val sum_n = sum + hd num_list
	in
	    if sum_n >= sum_target
	    then n - 1
	    else
		judge(sum_target, tl num_list, n+1, sum_n)
	end
  in
      judge(sum_target, num_list, 1 , 0)	   
  end

fun what_month(day : int) =
  let val intdays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
      1 + number_before_reaching_sum(day, intdays)
  end

fun month_range(day1 : int, day2 :int) =
  if day1 > day2
  then []
  else
      what_month(day1) :: month_range(day1+1, day2)

fun oldest(dates: (int*int*int) list) =
  if null dates
  then NONE
  else
      let
	  fun myoldest(dates: (int*int*int) list) =
	    if null (tl dates)
	    then hd dates
	    else
		let val tl_dates = myoldest(tl dates)
		in
		    if is_older(hd dates, tl_dates)
		    then hd dates
		    else tl_dates
		end
      in
	  SOME (myoldest(dates))
      end
	  
		
fun cumulative_sum(nums : int list) =
  if null nums
  then []
  else
      let
	  fun help(nums: int list, cumu : int) =
	    if null nums
	    then []
	    else
		(hd nums + cumu) :: help(tl nums, hd nums + cumu)
      in
	  help(nums, 0)
      end
	  

	  
fun get_int_nth(strs : int list, n : int) =
  if n = 1
  then hd strs
  else
      get_int_nth(tl strs, n - 1)

fun judge_leap(date:int*int*int) =
   if (#3 date) mod 400 = 0
   then true
   else if (#3 date mod 4 = 0) andalso (#3 date mod 100 <> 0)
   then true
   else false

fun reasonable_date(date : int*int*int) =
  let val isleap = judge_leap(date)	    
  in
      let val months = if isleap then [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
		       else
			   [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
      in
	  if #2 date <= 12 andalso #2 date >= 1
	  then 
	      if #1 date >= 1 andalso #1 date <= get_int_nth(months, #2 date)
	      then true
	      else
		  false
	  else
	      false  
      end
  end
      
		  
						     
						     
