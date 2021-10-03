type 'a tree = 
       Node of 'a tree * 'a  * 'a tree | 
       Null;;

let ultraleft tr =
    let res (a, b) = b
    in let rec pom tr =
                match tr with
                | Null -> (0, true)
                | Node (l, _, r) -> 
                    let (hl, resl) = pom l
                    in if resl then
                        let (hr, resr) = pom r
                        in if resr && hl >= hr then (hl + 1, true)
                            else (0, false)
                        else (0, false)
        in res (pom tr);;

let a = Node (Node (Node (Null, 0, Null), 0, Null), 0, Null);;
assert (ultraleft a = true);;
let b = Node (Node (Node (Null, 0, Null), 0, Null), 0, Node (Node (Null, 0, Null), 0, Null));;
assert (ultraleft b = true);;
let c = Node (Node (Null, 0, Null), 0, Node (Node (Null, 0, Null), 0, Null));;
assert (ultraleft c = false);;