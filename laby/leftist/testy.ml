open Leftist;;

let a = empty;;
let b = empty;;
assert (a = b);;
assert (is_empty a);;
assert (is_empty b);;

let a = add 1 a;;
assert (not(is_empty a));;

let c = join a b;;
assert (not(is_empty c));;
assert (a = c);;

let (a1, a2) = delete_min a;;
assert (is_empty a2);;
assert (a1 = 1);;

let a = add 0 a;;
let (a1, a2) = delete_min a;;
assert (a1 = 0);;

let a = add 2 a;;
let (a1, a2) = delete_min a;;
assert (a1 = 0);;

let b = join a a;;

let rec buduj x n =
    if n = 0 then x
    else buduj (join x x) (n - 1);;

let aa = buduj a 5;;

let aa = buduj a 20;;

let bb = join aa aa;;
let c = fst (delete_min bb);;
assert (c = 0);;
