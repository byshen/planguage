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
