open List;;

let widoczne lst =
    let suma = fold_left (+) 0 lst in
    rev (fst (fold_left (fun (l, pref) x -> ((if pref = suma - pref - x then x :: l else l), pref + x)) ([], 0) lst));;