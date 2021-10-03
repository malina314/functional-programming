open List;;

let fff (x, _, _, _) = x;; 

(* a jest male, b moze byc rzedu max_int *)
let safe_mult a b =
    if (max_int / b < a) then max_int
    else a * b;;

let prostokat lst =
    let rec f (w, i, j, s) x =
        match s with
        (* | [] -> (w, i + 1, []) *)
        | (h :: t) when x = fst h -> (w, i + 1, j + 1, s)
        | (h :: t) when x > fst h -> (w, i + 1, j + 1, (x, i) :: s)
        | (h :: t) when x < fst h -> f (max w (safe_mult (j - snd h) (fst h)), snd h, j, t) x
        | _ -> failwith "impossible"
    in fff ((fold_left (f) (0, 1, 1, [(0, 0)]) (rev (0 :: lst))));;

prostokat [1;1;1;3];;
prostokat [1;1;3;3];;
prostokat [1;4;3;3];;
prostokat [1;4;0;3];;
prostokat [4;5;1];;
prostokat [4;5;0];;
prostokat [max_int;max_int];;
prostokat [1;4;3;3];;
prostokat [3;3;4;1];;