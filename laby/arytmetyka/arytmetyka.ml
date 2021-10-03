(* autor: Mateusz Malinowski *)
(* reviewer: Ewa Majdaniuk *)

type wartosc = {
    min : float;
    max : float;
    zawiera_zero : bool; (* konce ma po obu stronach zera np (-2, 3) ale nie (0, 5) *)
    jest_suma: bool (* jest postaci (-inf, min)u(max;inf) *)
};;

exception Ujemne_p;;

(* wartosc_dokladnosc x p = x +/- p% *)
(* war.pocz.: p > 0 *)
let wartosc_dokladnosc x p =
    if p < 0. then raise Ujemne_p
    else 
        let (min, max) = (x -. x *. p /. 100., x +. x *. p /. 100.)
        in if x > 0. then {min; max; zawiera_zero = min < 0. && max > 0.; jest_suma = false}
            else {min = max; max = min; zawiera_zero = min < 0. && max > 0.; jest_suma = false};; (* jak x jest ujemny to na odwrot *)

exception X_wiekszy_od_y;;

(* wartosc_od_do x y = [x;y] *)
(* war.pocz.: x <= y *)
let wartosc_od_do x y =
    if x > y then raise X_wiekszy_od_y
    else {min = x; max = y; zawiera_zero = x < 0. && y > 0.; jest_suma = false};;

(* wartosc_dokladna x = [x;x] *)
let wartosc_dokladna x = {min = x; max = x; zawiera_zero = false; jest_suma = false};;

(* in_wartosc w x = x \in w // wartosc -> float -> bool *)
let in_wartosc w x =
    if classify_float w.min = FP_nan || classify_float w.max = FP_nan then false
    else if w.jest_suma then
        if x <= w.min || x >= w.max then true else false
    else
        if w.min <= x && x <= w.max then true else false;;

(* min_wartosc w = najmniejsza mozliwa wartosc w,   *)
(* lub neg_infinity jesli brak dolnego ograniczenia.*)
let min_wartosc x =
    if classify_float x.min = FP_nan || classify_float x.max = FP_nan then nan
    else if x.jest_suma then neg_infinity
    else x.min;;

(* max_wartosc w = najwieksza mozliwa wartosc w,    *)
(* lub infinity jesli brak gornego ograniczenia.    *)
let max_wartosc x =
    if classify_float x.min = FP_nan || classify_float x.max = FP_nan then nan
    else if x.jest_suma then infinity
    else x.max;;

(* srodek przedzialu od min_wartosc do max_wartosc, *)
(* lub nan jesli min i max_wartosc nie sa okreslone.*)
let sr_wartosc x = 
    if classify_float x.min = FP_nan || classify_float x.max = FP_nan then nan
    else if x.jest_suma then nan
    else (x.min +. x.max) /. 2.;;

let rec plus x y =
    if classify_float x.min = FP_nan || classify_float x.max = FP_nan then {min = nan; max = nan; zawiera_zero = false; jest_suma = false}
    else if classify_float y.min = FP_nan || classify_float y.max = FP_nan then {min = nan; max = nan; zawiera_zero = false; jest_suma = false}
    else if x.jest_suma && y.jest_suma then {min = neg_infinity; max = infinity; zawiera_zero = true; jest_suma = false}
    else if x.jest_suma then
        let (min, max) = (x.min +. y.max, x.max +. y.min)
        in if min >= max then {min = neg_infinity; max = infinity; zawiera_zero = true; jest_suma = false}
            else {min; max; zawiera_zero = min > 0. || max < 0.; jest_suma = true}
    else if y.jest_suma then plus y x (* dodawanie jest przemienne *)
    else let (min, max) = (x.min +. y.min, x.max +. y.max)
        in {min; max; zawiera_zero = min < 0. && max > 0.; jest_suma = false}

let minus x y =
    if classify_float x.min = FP_nan || classify_float x.max = FP_nan then {min = nan; max = nan; zawiera_zero = false; jest_suma = false}
    else if classify_float y.min = FP_nan || classify_float y.max = FP_nan then {min = nan; max = nan; zawiera_zero = false; jest_suma = false}
    else if x.jest_suma && y.jest_suma then {min = neg_infinity; max = infinity; zawiera_zero = true; jest_suma = false}
    else if x.jest_suma then
        let (min, max) = (x.min -. y.min, x.max -. y.max)
        in if min >= max then {min = neg_infinity; max = infinity; zawiera_zero = true; jest_suma = false}
            else {min; max; zawiera_zero = min > 0. || max < 0.; jest_suma = true}
    else if y.jest_suma then (* odejmowanie nie jest przemienne *)
        let (min, max) = (x.max -. y.max, x.min -. y.min)
        in if min >= max then {min = neg_infinity; max = infinity; zawiera_zero = true; jest_suma = false}
            else {min; max; zawiera_zero = min > 0. || max < 0.; jest_suma = true}
    else let (min, max) = (x.min -. y.max, x.max -. y.min)
        in {min; max; zawiera_zero = min < 0. && max > 0.; jest_suma = false}

let popraw x = (* skleja przedzial jesli przy mnozeniu w sytuacji np. razy_pom ((-inf,0),(1,+inf)) ((-inf,-1),(1,+inf))=((-inf,0),(0,inf)) *)
    if x.jest_suma && x.min = x.max then {min = neg_infinity; max = infinity; zawiera_zero = true; jest_suma = false}
    else x;;

let razy x y =
    let rec razy_pom x y = 
        if classify_float x.min = FP_nan || classify_float x.max = FP_nan then {min = nan; max = nan; zawiera_zero = false; jest_suma = false}
        else if classify_float y.min = FP_nan || classify_float y.max = FP_nan then {min = nan; max = nan; zawiera_zero = false; jest_suma = false}
        else if (x.min = 0. && x.max = 0.) || (y.min = 0. && y.max = 0.) then {min = 0.; max = 0.; zawiera_zero = false; jest_suma = false}
        else if x.jest_suma && y.jest_suma then
            if x.zawiera_zero || y.zawiera_zero then {min = neg_infinity; max = infinity; zawiera_zero = true; jest_suma = false} (* oba sa suma przedzialow i jest 0 *)
            else {min = max (x.min *. y.max) (x.max *. y.min); max = min (x.max *. y.max) (x.min *. y.min); zawiera_zero = false; jest_suma = true} (* oba sa suma przedzialow i nie ma 0 *)
        else if x.jest_suma && y.min >=0. then {min = x.min *. y.min; max = x.max *. y.min; zawiera_zero = x.zawiera_zero; jest_suma = true} (* x suma przedzialow, y dodatni *)
        else if x.jest_suma && y.max <=0. then {min = x.max *. y.max; max = x.min *. y.max; zawiera_zero = x.zawiera_zero; jest_suma = true} (* x suma przedzialow, y ujemny *)
        else if x.jest_suma then {min = neg_infinity; max = infinity; zawiera_zero = true; jest_suma = false} (* x suma przedzialow, y zawiera 0 *)
        else if y.jest_suma then razy_pom y x (* mnozenie jest przemienne *)
        else if x.min >= 0. && y.min >= 0. then {min = x.min *. y.min; max = x.max *. y.max; zawiera_zero = false; jest_suma = false} (* oba dodatnie *)
        else if x.min >= 0. && y.max <= 0. then {min = x.max *. y.min; max = x.min *. y.max; zawiera_zero = false; jest_suma = false} (* x dodatni, y ujemny *)
        else if x.max <= 0. && y.min >= 0. then {min = y.max *. x.min; max = y.min *. x.max; zawiera_zero = false; jest_suma = false} (* x ujemny, y dodatni; zamieniony x z y z przypadku wyzej *)
        else if x.max <= 0. && y.max <= 0. then {min = x.max *. y.max; max = x.min *. y.min; zawiera_zero = false; jest_suma = false} (* oba ujemne *)
        else if y.zawiera_zero then
            if x.min = neg_infinity || x.max = infinity then {min = neg_infinity; max = infinity; zawiera_zero = true; jest_suma = false} (* +-0 razy +-inf *)
            else if x.zawiera_zero then {min = min (x.min *. y.max) (x.max *. y.min); max = max (x.max *. y.max) (x.min *. y.min); zawiera_zero = true; jest_suma = false} (* oba zawieraja zera *)
            else if x.min >= 0. then {min = x.max *. y.min; max = x.max *. y.max; zawiera_zero = true; jest_suma = false} (* x dodatni, y zawiera 0 *)
            else if x.max <= 0. then {min = x.min *. y.max; max = x.min *. y.min; zawiera_zero = true; jest_suma = false} (* x ujemny, y zawiera 0 *)
            else razy_pom y x
        else razy_pom y x
    in popraw (razy_pom x y);;

let odwrotnosc x = (* odwrotnosc 0 nigdy sie nie wywola wiec nie ma problemu*)
    if x.zawiera_zero then 
        if x.jest_suma then {min = 1. /. x.max; max = 1. /. x.min; zawiera_zero = true; jest_suma = true}
        else {min = 1. /. x.min; max = 1. /. x.max; zawiera_zero = false; jest_suma = true}
    else if x.jest_suma then {min = 1. /. x.min; max = 1. /. x.max; zawiera_zero = true; jest_suma = false}
    else if x.min >= 0. then
        let max = if x.min = 0. then 1. /. 0. else 1. /. x.min
        in {min = 1. /. x.max; max; zawiera_zero = false; jest_suma = false}
    else
        let min = if x.max = 0. then 1. /. -0. else 1. /. x.max
        in {min; max = 1. /. x.min; zawiera_zero = false; jest_suma = false};;

let podzielic x y = (* mnozenie przez odwrotnosc *)
    if classify_float x.min = FP_nan || classify_float x.max = FP_nan then {min = nan; max = nan; zawiera_zero = false; jest_suma = false}
    else if classify_float y.min = FP_nan || classify_float y.max = FP_nan then {min = nan; max = nan; zawiera_zero = false; jest_suma = false}
    else if y.min = 0. && y.max = 0. then {min = nan; max = nan; zawiera_zero = false; jest_suma = false} (* x / 0 *)
    else if x.min = 0. && x.max = 0. then {min = 0.; max = 0.; zawiera_zero = false; jest_suma = false} (* 0 / y *)
    else if x.zawiera_zero && y.zawiera_zero then {min = neg_infinity; max = infinity; zawiera_zero = true; jest_suma = false} (* (-a,b) / (-c,d) *)
    else if x.jest_suma && y.jest_suma then {min = neg_infinity; max = infinity; zawiera_zero = true; jest_suma = false} (* (-inf,a)u(b,inf) / (-inf,c)u(d,inf) *)
    else if y.min = neg_infinity && y.max = infinity then {min = neg_infinity; max = infinity; zawiera_zero = true; jest_suma = false} (* x / (-inf,inf) *)
    else razy x (odwrotnosc y);;
