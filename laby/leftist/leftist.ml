(* Autor: Mateusz Malinowski, reviewer: Tomasz Nowak *)
type 'a queue = Null | El of 'a queue * 'a * 'a queue * int;; (* int trzyma wysokosc *)

let empty = Null;;

let is_empty q = (q = Null);;

let rec join q1 q2 =
    match q1, q2 with
    | x, Null -> x
    | Null, x -> x
    | El (l1, a1, r1, h1), El (_, a2, _, _) ->
        if a1 > a2 then
            join q2 q1
        else 
            let q = join r1 q2 in 
            match q with
            | Null -> failwith "Cannot happen"
            | El (_, _, _, h) -> 
                if h > h1 - 1 then
                    El (q, a1, l1, h + 1)
                else
                    El (l1, a1, q, h1);;
 
let add e q = join q (El (Null, e, Null, 1));;

exception Empty;;

let delete_min q =
    match q with
    | Null -> raise Empty
    | El (l, a, r, _) -> (a, join l r);;