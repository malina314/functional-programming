let rec funkcja_pom n i =
  if (i > n - 2) then
    false
  else
    if i * i mod n == 1 then
      true
    else
      funkcja_pom n (i + 1);;


let funkcja n =
  funkcja_pom n 2;;

Printf.printf "%B" (funkcja (read_int()));; print_string "\n";;