open List;;

let rec merge lista1 lista2 =
  match (lista1, lista2) with
    ([], []) -> [] |
    ([], _) -> lista2 |
    (_, []) -> lista1 |
    (head1 :: tail1, head2 :: tail2) -> 
      if head1 <= head2 then
        head1 :: merge tail1 lista2
      else  
        head2 :: merge lista1 tail2;;

    
(* merge [1;2;2;2;5] [3;4];; *)

let rec licz_pom lista ac p =
  if hd lista = p then
    try
      licz_pom (tl lista) ac (hd lista)
    with
      wyjatek -> ac
  else
    try
      licz_pom (tl lista) (ac + 1) (hd lista)
    with
      wyjatek -> ac + 1;;


let licz lista1 lista2 =
  let lista = merge lista1 lista2 in
    try licz_pom lista 0 (hd lista - 1)
    with w -> 0;;

print_int (licz [1;2;2;2;5] [3;4]);;
print_string "\n";;
(* licz [1; 1; 3; 4; 7] [2; 4; 6];;
licz [1] [];;
licz [] [1];;
licz [] [];; *)

(* let rec print_list = function 
  [] -> ()
  | e::l -> print_int e ; print_string " " ; print_list l *)