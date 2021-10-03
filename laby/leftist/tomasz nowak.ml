(* Autor: Tomasz Nowak, reviewer: Mateusz Malinowski *)
type 'a queue =
    | Null
    | Node of { value: 'a; depth: int; left: 'a queue; right: 'a queue }
 
let get_depth q = match q with
    | Node n -> n.depth
    | Null -> 0
 
exception Empty
let empty = Null
let is_empty q = match q with
    | Null -> true
    | _ -> false
 
let rec join queue1 queue2 = match queue1, queue2 with
    | x, Null
    | Null, x -> x
    | Node n1, Node n2 ->
        if n1.value > n2.value then
            join queue2 queue1
        else 
            let s1 = n1.left and s2 = join n1.right queue2 in
            let s1, s2 = if get_depth s1 < get_depth s2 then s2, s1 else s1, s2 in
            Node { value = n1.value; depth = get_depth s2 + 1; left = s1; right = s2 }
 
let add value q = join q (Node { value = value; depth = 1; left = Null; right = Null })
let delete_min q = match q with
    | Null -> raise Empty
    | Node n -> (n.value, join n.left n.right)