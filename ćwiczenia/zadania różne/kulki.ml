type kolor = Zielona | Czerwona;;

open List;;

let kulki lst =
    let f (w, i) kol =
        if i && kol = Zielona || not i && kol = Czerwona then (w, not i)
        else (w + 1, not i)
    in (fst (fold_left (f) (0, true) (rev lst))) / 2;;

(* let a = [Zielona; Zielona; Czerwona; Czerwona; Czerwona];;

kulki a;; *)

kulki [Czerwona; Zielona; Zielona; Zielona; Zielona; Czerwona; Czerwona; Zielona];;