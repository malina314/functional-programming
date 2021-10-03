(* Mateusz Malinowski *)

(* idea: drzewo przedzalowe z dodawaniem i maksimum *)
(* zlozonosc czasowa: nlogn *)
(* zlozonosc pamieciowa: nlogn *)
(* spoiler: nie dziala (program, nie idea) *)

type tree = Null | Node of tree * int * tree

let lazyy (Node (l, v, r)) = (* leniwe spychanie *)
    match l, r with
    | Null, Null -> Node (Node (Null, v, Null), 0, Node (Null, v, Null))
    | Node (ll, lv, lr), Null -> Node (Node (ll, lv + v, lr), 0, Node (Null, v, Null))
    | Null, Node (rl, rv, rr) -> Node (Node (Null, v, Null), 0, Node (rl, rv + v, rr))
    | Node (ll, lv, lr), Node (rl, rv, rr) -> Node (Node (ll, lv + v, lr), 0, Node (rl, rv + v, rr))

let rec add l r ll rr tree = (* dodajemy 1 na przedziale [l, r] do wierzcholka trzymajacego przedzial [ll, rr] *)
    match tree with
    | Null -> add l r ll rr (Node (Null, 0, Null))
    | Node (lt, vt, rt) ->
        if r < ll || l > rr then tree
        else if l <= ll && rr <= r then
            if vt <> 0 then let Node (nlt, nvt, nrt) = lazyy tree in Node (nlt, nvt + 1, nrt)
            else Node (lt, vt + 1, rt)
        else Node (add l r ll ((ll + rr) / 2) lt, vt, add l r ((ll + rr) / 2 + 1) rr rt)

let rec query l r ll rr tree = (* pytamy o max na przedziale [l, r] wierzcholek trzymajacy przedzial [ll, rr] *)
    match tree with
    | Null -> 0
    | Node (lt, vt, rt) ->
        if r < ll || l > rr then 0
        else if l <= ll && rr <= r then vt
        else max (query l r ll ((ll + rr) / 2) lt) (query l r ((ll + rr) / 2 + 1) rr rt)

open List;;

let wirus lst =
    let tree = fold_left (fun tr (p, k) -> add p (k - 1) 1 (1 lsl 61) tr) Null lst in (* budujemy drzewo, dodajemy na przedziale [p, k - 1] dlatego ze wsiadajacy i wysiadajacy na jednym przystanku nie maja ze soba kontaktu *)
    let f a (p, k) =
        (query p (k - 1) 1 (1 lsl 61) tree - 1) :: a (* bierzemy maksimum na przedziale i odejmujemy 1 bo dana osoba tez jest dodana na tym przedziale do drzewa *)
    in
    rev (fold_left f [] lst);;

assert (wirus [(1,8);(2,5);(4,7);(3,4);(4,5);(6,7);(8,9)] = [5; 4; 4; 2; 3; 2; 0]);;

(* co nie dziala? *)
(* w momencie dodawania powinnismy jeszcze zrobic update w gore a to wymaga przebudowy wiekszosci kodu i juz nie starczy mi czasu *)


(* mozna to zrobic inaczej *)
(* skalujemy, niech p' i k' to p i k po przeskalowaniu, bierzemy tablice, dla kazdych p', k' robimy t[p']++; t[k']--; potem sumy prefiksowe
   wynik to maksimum na przedziale [p', k'-1], zeby to dla wszystkich na raz jakos szybko zrobic to chyba i tak potem trzeba drzewo na tym
   zbudwac ale bedzie punkt przedzial; zlozonosc: nlogn *)

(* na rozw lepsze nie mam pomyslu (wydaje mi sie ze sie nie da) *)