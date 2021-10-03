type 'a tree = Null | Node of 'a tree * 'a * 'a tree;;

let srednica tr =
    let (x, y) =
        let rec pom tr =
            match tr with
            | Null -> (0, 0) (* najdluzsza sciezka, srednica *)
            | Node (l, _, r) ->
                let ((a, sa), (b, sb)) =  (pom l, pom r)
                in (max a b + 1, max (a + b) (max sa sb))
        in pom tr
    in y;;

let a = Node (Null, 1, Null);;
let b = Node (a, 2, a);;