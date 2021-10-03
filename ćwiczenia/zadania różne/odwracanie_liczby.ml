let f2 x y = x * int_of_float (10. ** y);;

let rec f1 x = 
  if x > 0 then
    f2 (x mod 10) (floor (log10 (float_of_int x))) + f1 (x / 10)
  else
    0
;;
