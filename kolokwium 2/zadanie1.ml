(* Mateusz Malinowski *)

type expr = Add of expr * expr | Var of string

let rec fold_expr f_add f_var = function
    | Add (l, r) -> f_add (fold_expr f_add f_var l) (fold_expr f_add f_var r)
    | Var z -> f_var z

let st = fold_expr (fun ls rs -> max ls (1 + rs)) (fun _ -> 1)



(* zlozonosc czasowa theta(n), gdzie n to liczba wierzcholkow w drzewie, poniewaz kazdy wierzcholek przetworzymy dokladnie raz *)
(* zlozonosc pamieciowa theta(n), gdzie n to liczba wierzcholkow w drzewie, poniewaz wynikowe drzewo ma dokladnie tyle samo wierzcholkow co poczatkowe *)
let optymalizuj stos =
    (* funkcje pomocnicze zwracaja pare (wysokosc, wyrazenie) (int * expr) *)
    let f_add (lh, lp) (rh, rp) = (* lh, rh - wysokosci poddrzew (int); lp, rp - lewe i prawe poddrzewo (expr) *)
        if lh < rh then (rh, Add (rp, lp)) (* jezeli prawe poddrzewo jest wyzsze to zamieniamy; wysokosc to rh bo poniewaz lh < rh to rh >= lh + 1 *)
        else (max lh (rh + 1), Add (lp, rp))
    and f_var x = (1, Var (x))
    in
    snd (fold_expr f_add f_var stos);;
(* czemu to dziala? zauwazmy, ze skoro h = max l (r + 1) to chcemy zeby prawe poddrzewo bylo nie wieksze niz lewe,
   wiec jezeli prawe jest wieksze to zamieniamy a w przeciwnym wypadku nie;
   drzewo minimalizujemy od dolu wiec rozwazajac wierzcholek v jego poddrzewa maja minimalne mozliwe wysokosci *)


assert (optymalizuj (Add (Var "a", Add (Var "b", Var "c"))) = Add (Add (Var "b", Var "c"), Var "a"));;