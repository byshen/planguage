fun append(xs : list, ys : list) =
  if null xs
  then ys
  else
      (hd xs) :: append((tl xs), ys)
		       
