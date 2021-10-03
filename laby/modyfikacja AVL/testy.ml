open ISet;;

let a = empty;;
let b = add (3, 5) a;;
assert (elements b = [(3,5)]);;
let c = add (7, 10) b;;
assert (elements c = [(3,5); (7,10)]);;
let d = add (6, 8) c;;
assert (elements d = [(3,10)]);;
let e = add (6, 6) c;;
assert (elements e = [(3,10)]);;
let f = remove (6, 8) e;;
assert (elements f = [(3,5); (9,10)]);;
let g = remove (0, 100) f;;
assert (is_empty g);;

let s = add (1, 1) (add (15, 16) (add (10, 14) (add (6, 9) empty)));;
assert (is_empty (remove (1, 20) s));;
let s = add (3, 4) (add (8, 10) (add (15, 20) empty));;
assert (below 3 s = 1);;
let s = add (-min_int, max_int) empty;;
assert (below min_int s = 1);;

let b = empty;;
let b = add (-10, 5) b;;
let b = add (10, 34) b;;
let b = add (22, 40) b;;
assert (below max_int (add (0, max_int) empty) = max_int);;

(* testy wydajnosciowe operacji below *)

let a = empty;;
let rec f n =
    if n < 100000 then
        add (3 * n, 3 * n + 1) (f (n + 1))
    else
        add (3 * n, 3 * n + 1) a

let a = f 0;;

let rec b n =
    if n < 380000 then
        (b (n + 1); below n a)
    else
        below n a;;

b 280000;;