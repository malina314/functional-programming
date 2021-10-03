open List;;

let sumy lst =
    let pom a x =
        match a with
        | [] -> [x]
        | (head :: tail) -> ((head + x) :: a)
    in rev (fold_left (pom) [] lst);;


let sumy_fold_rajtem_XD lst =
    let pom x a =
        match a with
        | [] -> [x]
        | (head :: tail) -> ((head + x) :: a)
    in rev (fold_right (pom) (rev lst) []);;


let sumy_w_jednej_linijce lst = let pom a x = match a with | [] -> [x] | (head :: tail) -> ((head + x) :: a) in rev (fold_left (pom) [] lst);;