let rec fff_ogonowo n pp ak =
  if pp <= n then
     fff_ogonowo n (pp * 5) (ak + n / pp)
  else
    ak;;

let ile_zer n =
  fff_ogonowo n 5 0;;


print_int (ile_zer (read_int()));;
print_string "\n";;