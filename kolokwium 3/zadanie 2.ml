(* Mateusz Malinowski *)

(* złożoność czasowa: O(n * m * log* (m * n)) - find & union *)
(* złożoność pamięciowa: O(n * m) - tworzone są tablice m x n, koszt rekurencji find asymptotycznie jest pomijalny *)

let rec find (x, y) rep =
    if rep.(x).(y) <> (x, y) then
        (rep.(x).(y) <- find rep.(x).(y) rep;
        rep.(x).(y))
    else
        (x, y);;

let union (ax, ay) (bx, by) rep rank =
    if rank.(ax).(ay) = rank.(bx).(by) then
        (rank.(ax).(ay) <- (rank.(ax).(ay) + 1);
        rep.(bx).(by) <- rep.(ax).(ay))
    else if rank.(ax).(ay) > rank.(bx).(by) then
        rep.(bx).(by) <- rep.(ax).(ay)
    else
        rep.(ax).(ay) <- rep.(bx).(by);;


let labirynt m n lst =
    let rep = Array.make_matrix m n (0, 0) in
    let rank = Array.make_matrix m n 0 in
    for i = 0 to m - 1 do
        for j = 0 to n - 1 do
            rep.(i).(j) <- (i, j); (* inicjowanie reprezentantów *)
        done;
    done;
    let f acc (x, y, kierunek) = (* funkcja pomocnicza dla fold_left *)
        if kierunek then (* poziome *)
            let ra = find (x, y) rep in
            let rb = find (x, y - 1) rep in
            if ra = rb then
                (x, y, kierunek) :: acc
            else
                (union ra rb rep rank;
                acc)
        else  (* pionowe *)
            let ra = find (x, y) rep in
            let rb = find (x - 1, y) rep in
            if ra = rb then
                (x, y, kierunek) :: acc
            else
                (union ra rb rep rank;
                acc)
    in
    List.fold_left f [] (List.rev lst);;


labirynt 3 2 [(2,1,false); (2,1,true); (2,0,false); (1,1,true); (0,1,true); (1,1,false); (1,0,false) ];;

(* Pomysł jest taki, że przeglądamy listę od tyłu (czyli mamy wszystkie krawędzie i je usuwamy)
 * i łączymy pola oddzielane krawędzią. Jeżeli pola były w różnych spójnych to znaczy, że dodanie
 * tej krawędź rozspójniło labirynt i należy ją odrzucić. Jeżeli pola były w tej samej spójnej 
 * to znaczy, że dodanie tej krawędzi nie rozspójnia labiryntu i możemy ją zakumulować w wyniku *)