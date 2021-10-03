(* Mateusz Malinowski *)

(* złożoność czasowa: O(n), gdzie n to długość listy *)
(* złożoność pamięciowa: O(1) *)

type 'a option = None | Some of 'a

type 'a elem = {v: 'a; mutable prev: 'a lista; mutable next: 'a lista}
and 'a lista = 'a elem option;;

let get (Some (x)) = x;;

let poodwracaj lstd lst = (* lstd - lista dwukierunkowa, lst - lista intów *)
    let wynik = ref ({v = 0; prev = None; next = None}) in (* muszę czymś zainicjować *)
    let tak = ref true in (* potrzebne, żeby wynik ustawić tylko raz *)
    let f (p, a) x = (* p - poprzednik, ten, od którego zaczynamy obracać segment, x - dłgość obracanego segmentu *)
        let first_elem = a in
        let ra = ref a in
        let next_a = ref (get a).next in
        for i = 1 to x do
            next_a := (get !ra).next;
            (get !ra).next <- (get !ra).prev;
            (get !ra).prev <- !next_a;
            if i < x then
                ra := !next_a;
        done;
        (get !ra).prev <- p;
        (get first_elem).next <- !next_a;
        if !tak then
            (tak :=  false;
            wynik := (get !ra););
        if p <> None then
            (get p).next <- !ra;
        (first_elem, !next_a)
    in
    List.fold_left f (None, lstd) lst;
    !wynik;;



let rec d1 = Some {v = 4; prev = None; next = d2}
    and d2 = Some {v = 5; prev = d1; next = d3}
    and d3 = Some {v = 6; prev = d2; next = d4}
    and d4 = Some {v = 7; prev = d3; next = d5}
    and d5 = Some {v = 8; prev = d4; next = d6}
    and d6 = Some {v = 9; prev = d5; next = None};;

let d = d1;;
let wynik = poodwracaj d [3; 2; 1];;