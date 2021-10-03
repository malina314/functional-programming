type 'a tree = Node of 'a * 'a tree list;;

let rec interuj func lst def agreguj =
    match lst with
    | [] -> def
    | (h :: t) -> agreguj (func h) (interuj func t def agreguj);;

let centroid tr =
    let rec find_size (Node (_, lst)) = interuj find_size lst 1 (+)
    in let size = find_size tr
        in let rec centroid_pom (Node (a, lst)) =
                let (czy, co, ile) = interuj centroid_pom lst (false, Node (a, lst), 1) (fun (a1, b1, c1) (a2, b2, c2) -> 
                    if a1 then (true, b1, c1 + c2)
                    else if a2 then (true, b2, c1 + c2)
                    else (false, b1, c1 + c2))
                in if czy then (true, co, ile)
                    else if ile * 2 >= size then (true, Node (a, lst), ile)
                    else (false, Node (a, lst), ile)
            in let f (a, b, c) = b
            in f (centroid_pom tr);;



(* let rec find_size (Node (_, lst)) = interuj find_size lst 1 (+);; *)

let b = Node (1, [Node (2, []); Node (3, []); Node (4, []); Node (5, [])]);;
let c = Node (1, [Node (2, [Node (3, []); Node (4, []); Node (5, [])]); Node (6, []); Node (7, []); Node (8, [])]);;
let d = Node (1, [Node (2, [Node (3, []); Node (4, []); Node (5, [])]); Node (6, []); Node (7, [])]);;

centroid d;;
