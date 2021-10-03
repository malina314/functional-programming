open List;;

let rec double lista =
  if lista = [] then
    []
  else
    (hd lista) :: ((hd lista) :: double (tl lista));;


double [1;2;3];;