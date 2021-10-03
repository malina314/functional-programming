(* autor: Mateusz Malinowski *)
(* reviewer: Paweł Balawender (gr. 2)*)

(*
 * ISet - Interval sets
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl,
 * Jacek Chrzaszcz, Mateusz Malinowski, Paweł Balawender
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

type t =
    | Empty
    | Node of t * int * int * t * int * int (* lewy, poczatek, koniec, prawy, wysokosc, liczba elementow (suma dlugosci wszystkich przedzialow w drzewie) *)

let height = function
    | Node (_, _, _, _, h, _) -> h
    | Empty -> 0

let size = function
    | Node (_, _, _, _, _, s) -> s
    | Empty -> 0

let calculate_size l lv rv r = (* oblicza liczbe elementow w drzewie uwazajac na przekorczenie max_int *)
    let s = size l + size r + rv - lv + 1 in
    if s < max (size l) (size r) then max_int
    else s

let make l lv rv r = Node (l, lv, rv, r, max (height l) (height r) + 1, calculate_size l lv rv r) (* tworzy wierzcholek, wysokosci l i r nie roznia sie wiecej niz o 2 *)

let bal l lv rv r = (* rownowazy drzewo, gdy roznica wysokosci jest wieksza niz 2, O(log(n)) *)
    let hl = height l in
    let hr = height r in
    if hl > hr + 2 then
        match l with
        | Node (ll, llv, lrv, lr, _, _) ->
            if height ll >= height lr then make ll llv lrv (make lr lv rv r)
            else
                (match lr with
                | Node (lrl, lrlv, lrrv, lrr, _, _) ->
                    make (make ll llv lrv lrl) lrlv lrrv (make lrr lv rv r)
                | Empty -> assert false)
        | Empty -> assert false
    else if hr > hl + 2 then
        match r with
        | Node (rl, rlv, rrv, rr, _, _) ->
            if height rr >= height rl then make (make l lv rv rl) rlv rrv rr
            else
                (match rl with
                | Node (rll, rllv, rlrv, rlr, _, _) ->
                    make (make l lv rv rll) rllv rlrv (make rlr rlv rrv rr)
                | Empty -> assert false)
        | Empty -> assert false
    else
        Node (l, lv, rv, r, max hl hr + 1, calculate_size l lv rv r)

let rec min_elt = function (* zwraca najmniejszy przedzial, O(log(n)) *)
    | Node (Empty, lv, rv, _, _, _) -> (lv, rv)
    | Node (l, _, _, _, _, _) -> min_elt l
    | Empty -> failwith "Not_found_min_elt"

let rec remove_min_elt = function (* usuwa najmniejszy przedzial, O(log(n)) *)
    | Node (Empty, _, _, r, _, _) -> r
    | Node (l, lv, rv, r, _, _) -> bal (remove_min_elt l) lv rv r
    | Empty -> invalid_arg "PSet.remove_min_elt"

let rec max_elt = function (* zwraca największy przedzial, O(log(n)) *)
    | Node (_,lv, rv, Empty, _, _) -> (lv, rv)
    | Node (_, _, _, r, _, _) -> max_elt r
    | Empty -> failwith "Not_found_max_elt"

let rec remove_max_elt = function (* usuwa największy przedzial, O(log(n)) *)
    | Node (l, _, _, Empty, _, _) -> l
    | Node (l, lv, rv, r, _, _) -> bal l lv rv (remove_max_elt r)
    | Empty -> invalid_arg "PSet.remove_max_elt"

let merge t1 t2 = (* laczy 2 drzewa, O(log(n)) *)
    match t1, t2 with
    | Empty, _ -> t2
    | _, Empty -> t1
    | _ ->
        let (lv, rv) = min_elt t2 in
        bal t1 lv rv (remove_min_elt t2)

let empty = Empty

let is_empty x = x = Empty

let cmp x lv rv = (* porownuje x z przedzialem, zwraca 0 gdy x nalezy, -1 gdy jest mniejszy, 1 gdy jest wiekszy *)
    if x > rv then 1
    else if x < lv then -1
    else 0

let rec add_one x y = function (* dodaje przedzial, ktory nie ma czesci wspolnej ani nie sosiaduje z zadnym dotychczasowym przedzialem, O(log(n)) *)
    | Node (l, lv, rv, r, _, _) ->
        if x > rv then bal l lv rv (add_one x y r)
        else if y < lv then bal (add_one x y l) lv rv r
        else failwith "should not have happend"
    | Empty -> Node (Empty, x, y, Empty, 1, calculate_size Empty x y Empty)

let rec join l lv rv r = (* zlacza 2 drzewa w jeden wierzcholek, tak zeby byl zrownowazony, O(log(n)) *)
    match (l, r) with
    | (Empty, _) -> add_one lv rv r
    | (_, Empty) -> add_one lv rv l
    | (Node(ll, llv, lrv, lr, lh, _), Node(rl, rlv, rrv, rr, rh, _)) ->
        if lh > rh + 2 then bal ll llv lrv (join lr lv rv r)
        else if rh > lh + 2 then bal (join l lv rv rl) rlv rrv rr
        else make l lv rv r

let safe_add x y set = (* funkcja pomocnicza dla split, jezeli przedzial jest sensowny to dodaje go do drzewa *)
    if y < x then set
    else add_one x y set;;

let split x set = (* O(log(n)) *)
    let rec loop x = function
        | Empty -> (Empty, false, Empty)
        | Node (l, lv, rv, r, _, _) ->
            let c = cmp x lv rv in
            if c = 0 then
                if x = min_int then (Empty, true, safe_add (x + 1) rv r)
                else if x = max_int then (safe_add lv (x - 1) l , true, Empty)
                else (safe_add lv (x - 1) l , true, safe_add (x + 1) rv r)
            else if c < 0 then
                let (ll, pres, rl) = loop x l in (ll, pres, join rl lv rv r)
            else
                let (lr, pres, rr) = loop x r in (join l lv rv lr, pres, rr)
    in
    loop x set

let add (x, y) set = (* usuwamy z drzewa wszystkie przedzialy majace czesc wspolna z dodawanym oraz przedzialy sasiadujace, dodajemy *)
    if (y < x) then failwith "invalid_interval"
    else
        let (l, _, r) = split x set in
        let (l2, x2) = 
            if l = Empty then (l, x)
            else
                let (maxl, maxr) = max_elt l in
                if maxr + 1 = x then (remove_max_elt l, maxl)
                else (l, x)
        and (r2, y2) = 
            let (_, _, rtmp) = split y r in
            if rtmp = Empty then (rtmp, y)
            else
                let (minl, minr) = min_elt rtmp in
                if y + 1 = minl then (remove_min_elt rtmp, minr)
                else (rtmp, y)
        in
        add_one x2 y2 (merge l2 r2)

let remove (x, y) set = (* O(log(n)) *)
    let (l, _, r) = split x set in
    let (_, _, r2) = split y r in
    merge l r2

let mem x set = (* O(log(n)) *)
    let rec loop = function
        | Node (l, lv, rv, r, _, _) ->
            let c = cmp x lv rv in
            c = 0 || loop (if c < 0 then l else r)
        | Empty -> false
    in
    loop set

let iter f set = (* O(n) *)
    let rec loop = function
        | Empty -> ()
        | Node (l, lv, rv, r, _, _) -> loop l; f (lv, rv); loop r
    in
    loop set

let fold f set acc = (* O(n) *)
    let rec loop acc = function
        | Empty -> acc
        | Node (l, lv, rv, r, _, _) ->
            loop (f (lv, rv) (loop acc l)) r 
    in
    loop acc set

let elements set = (* O(n) *)
    let rec loop acc = function
        | Empty -> acc
        | Node (l, lv, rv, r, _, _) -> loop ((lv, rv) :: loop acc r) l
    in
    loop [] set

let below x set = (* O(log(n)) *)
    let (l, flag, _) = split x set in
    let s = size l in
    if s = max_int then s
    else if flag then s + 1
    else s;;
