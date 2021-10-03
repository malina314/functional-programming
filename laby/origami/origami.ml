(* autor: Mateusz Malinowski *)
(* reviewer: Joanna Dagil *)

type point = float * float
type kartka = point -> int

let epsilon = 1e-12

(* zwraca 1 gdy punkt C jest z lewej strony prostej AB, 0 gdy lezy na prostej, -1 gdy lezy z prawej *)
let czy_z_lewej (cx, cy) ((ax, ay), (bx, by)) =
    let c = (cx -. ax) *. (by -. ay) -. (cy -. ay) *. (bx -. ax) in
    if -.epsilon <= c && c <= epsilon then 0
    else if  c < 0. then 1
    else -1

(* przyjmuje 2 punkty i zwraca wektor swobodny *)
let wektor (ax, ay) (bx, by) = ((bx -. ax), (by -. ay))

(* przyjmuje wektor swobodny i zwraca wektor swobodny do niego prostopadly *)
let wektor_prostopadly (x, y) = (y, -.x)

(* przyjmuje wektor swobodny i zwraca jego dlugosc *)
let len (x, y) = sqrt ((x *. x) +. (y *. y))

(* normalizuje wektor swobodny *)
let normalizuj ((x, y) as v) =
    let k = len v in
    (x /. k, y /. k)

(* warosc bezwzgledna dla floatow *)
let abs x = if x < 0. then -.x else x

(* zwraca wspolczynniki A, B, C postaci ogolnej prostej Ax + By + C = 0 *)
let postac_ogolna ((ax, ay), (bx, by)) = (ay -. by, bx -. ax, ax *. (by -. ay) -. ay *. (bx -. ax))

(* zwraca punkt C' symetryczny do punktu C wzgledem prostej AB *)
let symetria (cx, cy) ((ax, ay), (bx, by)) =
    let ((vx, vy) as v) = normalizuj (wektor_prostopadly (wektor (ax, ay) (bx, by))) in
    let (a, b, c) = postac_ogolna ((ax, ay), (bx, by)) in
    let odl = abs (a *. cx +. b *. cy +. c) /. sqrt (a *. a +. b *. b) in
    ((cx +. 2. *. vx *. odl), (cy +. 2. *. vy *. odl));;

let prostokat (x1, y1) (x2, y2) = function (px, py) ->
    if x1 -. epsilon <= px && px <= x2 +. epsilon && y1 -. epsilon <= py && py <= y2 +. epsilon then 1
    else 0

let kolko (x, y) r = function (px, py) ->
    if (px -. x) *. (px -. x) +. (py -. y) *. (py -. y) <= r *. r +. epsilon then 1
    else 0

let zloz p1 p2 kar = function p ->
    let c = czy_z_lewej p (p1, p2) in
    if c = 1 then kar p + kar (symetria p (p1, p2))
    else if c = 0 then kar p
    else 0

let compose f g = function x -> f (g x)

let skladaj lst kar = List.fold_left (fun a (x1, x2) -> compose (zloz x1 x2) a) (fun x -> x) lst kar;;
