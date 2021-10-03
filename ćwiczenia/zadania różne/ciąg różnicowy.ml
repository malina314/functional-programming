open List

let roznicowe lst =
    match lst with
    | [] -> []
    | [_] -> []
    | head :: tail ->
        rev (fst (fold_left (fun (a, h) x -> ((x - h) :: a), x) ([], head) tail));;

let ciag_ciagow lst =
    match lst with
    | [] -> []
    | _ -> rev (fst (fold_left (fun (w, o) _ -> let li = roznicowe o in ((li :: w), li)) ([], lst) (tl lst)));;