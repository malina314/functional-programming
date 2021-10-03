open List;;

let list_to_int x =
    let rec pom x a k =
        match x with
        | [] -> a
        | h::t -> pom t (a + h * k) (k * (-2))
    in pom (rev x) 0 (-1);;

let int_to_list x =
    