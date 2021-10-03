(* autor: Mateusz Malinowski *)
(* reviewer: Bartek Sadlej *)

open PMap
open List

exception Cykliczne

(* v - wierzcholek, mapa - mapa odwiedzonych, wynik - lista-akumulator wyniku, listy - mapa z listami sasiedztwa wszystkich wierzcholkow *)
(* zwraca pare (aktualna mapa owiedzonych, lista posortowanych dotychczas wierzcholkow) *)
(* stan odwiedzonych: 0 - nieodwiedzony, 1 - wierzcholek przetwarzany (dfs wszedl do niego ale jeszcze nie wyszedl), 2 - wierzcholek przetworzony (jest juz na wynikowej liscie) *)
let rec dfs v mapa wynik listy =
    let stan = try PMap.find v mapa with Not_found -> 0 in
    if stan = 0 then (* wchodzimy do wierzcholka po raz pierwszy *)
        let lst = try PMap.find v listy with Not_found -> [] in (* lst - lista sasiadow wierzcholka v *)
        let (m2, w2) = fold_left (fun (m, w) x -> dfs x m w listy) (add v 1 mapa, wynik) lst in (* ustawiamy w mapie stan wierzcholka v na 1 i wywolujemy rekurencyjnie dfs na wszystkich sasiadach *)
        (add v 2 m2, v :: w2) (* ustawiamy w mapie stan wierzcholka v na 2 i dodajemy go do wyniku *)
    else if stan = 1 then (* w grafie jest cykl *)
        raise Cykliczne
    else (* wierzcholek byl juz rozpatrzony i jest w wyniku *)
        (mapa, wynik)

let topol graf =
    let listy = fold_left (fun a (v, l) -> add v (try l @ (PMap.find v a) with Not_found -> l) a) empty graf in (* dodawanie list sasiedztwa do mapy *)
    snd (fold_left (fun (m, w) (v, _) -> dfs v m w listy) (empty, []) graf);; (* wywolanie dfs dla kazdego wierzcholka a_i *)
