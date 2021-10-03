open Arytmetyka;;

let zero = wartosc_od_do 0. 0.;;
let jeden = wartosc_od_do 1. 1.;;
assert (zero = wartosc_dokladna 0.);;
assert (jeden = wartosc_dokladna 1.);;
assert (in_wartosc (podzielic zero zero) 0. = false);;

let minusjedenjeden = wartosc_od_do (-1.) 1.;;
let minusdwatrzy = wartosc_od_do (-2.) 3.;;
let piec = wartosc_od_do 5. 5.;;
let a = podzielic jeden minusjedenjeden;; (* (-inf,-1)u(1,inf) *)
let b = podzielic jeden minusdwatrzy;; (* (-inf,-1/2)u(1/3,inf) *)
let c = plus a piec;; (* (-inf,4)u(6,inf) *)

let xxx = wartosc_od_do (-3.) (-1.);;

assert (in_wartosc c 3.);;
assert (in_wartosc c 4.);;
assert (in_wartosc c 6.);;
assert (in_wartosc c 7.);;
assert (in_wartosc c 4.00001 = false);;
assert (in_wartosc c 5.99999 = false);;
assert (podzielic zero b = zero);;

let minusinfinf = plus a b;;
let d = podzielic a minusinfinf;;
let e = podzielic minusjedenjeden minusjedenjeden;;

assert (d = e);;
assert (podzielic piec minusinfinf = minusinfinf);;
assert (podzielic jeden minusinfinf = minusinfinf);;
assert (podzielic a minusinfinf = minusinfinf);;
assert (podzielic b minusinfinf = minusinfinf);;
assert (podzielic c minusinfinf = minusinfinf);;
assert (podzielic zero minusinfinf = zero);;

let f = minus a piec;; (* (-inf,-6)u(-4,inf) *)

assert (razy c f = minusinfinf);;

let zerojeden = wartosc_od_do 0. 1.;;
let minusjeden = wartosc_dokladna (-1.);;
let j = podzielic jeden zerojeden;; (* (1,inf) *)
let k = minus zerojeden jeden;; (* (0,inf) *)
let l = razy j minusjeden;; (* (-inf,-1) *)
let m = plus l piec;; (* (-inf,4) *)

assert (in_wartosc j 1.);;
assert (in_wartosc j 10.);;
assert (in_wartosc j 1.1);;
assert (in_wartosc j 0.999999 = false);;
assert (in_wartosc l (-1.));;
assert (in_wartosc l (-10.));;
assert (in_wartosc l (-1.1));;
assert (in_wartosc l (-0.999999) = false);;

assert (plus j l = minusinfinf);;
assert (plus j k = minusinfinf = false);;
assert (razy j k = minusinfinf = false);;
assert (razy j l = l);;
assert (razy j m = minusinfinf);;
assert (razy m minusjeden = razy minusjeden m);;

let r1 = razy 
                    (wartosc_od_do (-5.4) (5.4) )
                    (wartosc_dokladnosc (-6.) (7.6));;
let zero = wartosc_od_do 0. 0.;;
assert (zero = wartosc_dokladna (0.));;

let b = podzielic r1 zero;;
let c = wartosc_od_do (0.) (8.);;

let a =
    in_wartosc (
        podzielic 
            (podzielic r1 zero)
            (wartosc_od_do (0.) (8.))
    ) (0.2);;
assert (a = false);;



let a = in_wartosc ( razy ( razy ( podzielic ( podzielic ( wartosc_od_do (7.6) (8.4) ) ( wartosc_od_do (-10.) (7.6) ) ) ( wartosc_dokladna (-2.8) ) ) ( wartosc_od_do (0.) (2.2) ) ) ( wartosc_dokladna (6.8) ) ) (-1.6);;
assert (a = true);;

let a = in_wartosc ( podzielic ( podzielic ( razy ( wartosc_dokladnosc (-1.2) (3.4) ) ( podzielic ( wartosc_od_do (0.) (4.4) ) ( wartosc_od_do (-5.8) (5.) ) ) ) ( wartosc_od_do (1.4) (3.8) ) ) ( wartosc_dokladnosc (-8.6) (9.8) ) ) (-4.2);;
assert (a = true);;

let a = max_wartosc ( podzielic ( wartosc_od_do (6.) (8.) ) ( podzielic ( wartosc_od_do (0.) (6.) ) ( wartosc_od_do (-7.) (4.) ) ) ) ;;
assert (a = infinity);;

let a = sr_wartosc ( razy ( podzielic ( podzielic ( wartosc_od_do (-8.) (0.) ) ( wartosc_dokladnosc (5.) (5.) ) ) ( wartosc_od_do (-10.) (5.) ) ) ( podzielic ( wartosc_dokladnosc (-10.) (1.) ) ( wartosc_od_do (-8.) (0.) ) ) ) ;;
assert ((classify_float a) == FP_nan);;


let aa = plus ( wartosc_dokladnosc (1.) (0.) ) ( razy ( wartosc_dokladna (8.) ) ( wartosc_od_do (-7.) (-6.) ) );;
let bb = podzielic ( minus ( minus ( wartosc_dokladnosc (-4.) (0.) ) ( wartosc_dokladnosc (0.) (6.) ) ) ( wartosc_dokladna (-6.) ) ) ( wartosc_od_do (-5.) (8.) );;


let a = min_wartosc ( podzielic ( wartosc_dokladnosc (-3.) (2.) ) ( podzielic ( minus ( aa ) ( bb ) ) ( wartosc_od_do (-9.) (1.) ) ) ) ;;
assert (a = neg_infinity);;