let iloczyn_wektorowy a b c = 
  match (a, b, c) with
    ((xA, yA), (xB, yB), (xC, yC)) -> xA * yB - xB * yA + xB * yC - xC * yB + xC * yA - xA * yC;;

let iloczyn_pom a b =
  match (a, b) with
    ((pA, pB), (pC, pD)) -> (iloczyn_wektorowy pA pB pC) * (iloczyn_wektorowy pA pB pD);;

let f_pom a b =
  match (a, b) with
    ((xA, yA), (xB, yB), (xC, yC), (xD, yD)) -> max xA xB < min


let przeciecie a b =
  (* if punkt_wspolny_odc a b then 
    true *)
  if if iloczyn_pom a b > 0 || iloczyn_pom b a > 0 then
    false
  else
    if iloczyn_pom a b <= 0 && iloczyn_pom b a <= 0 then
      true
    else
      ;;


(* Printf.printf "%B" (przeciecie ((1, 2), (3, 4)) ((1, 2), (3, 5)));; print_string "\n";;
Printf.printf "%B" (przeciecie ((1, 2), (3, 4)) ((1, 3), (3, 5)));; print_string "\n";;
Printf.printf "%B" (przeciecie ((1, 2), (3, 4)) ((3, 4), (3, 5)));; print_string "\n";; *)
(* let max a b =
  if a > b then a
  else b;; *)

print_int (max 1 2);;


(* let give_x_y a b =
  match (a, b) with 
    ((xa, ya), (xb, yb)) -> print_int (xa + ya + xb + yb);;


give_x_y (3, 5) (3, 6);; *)