(* Mateusz Malinowski *)

open List;;

(* let b = [1;1;0;1];; *)

let bonifacy n b = (* n - z tresci zadania, b - z tresci zadania  *)
    match n with
    | 0 -> 0
    | 1 -> 1
    | 2 -> 1
    | _ ->
        let rec pom i n (head_b :: tail_b) b2 a1 a2 a3 = (* i - indeks, n - z tresci zadania, (head_b :: tail_b) - glowa i ogon aktualnego sufiksu listy b, b2 - poczatkowa listy b, a1 - x_(i-1), a1 - x_(i-2), a1 - x_(i-3) *)
            let b = if tail_b = [] then b2 else tail_b and (* jezeli rozpatrujemy ostatni element listy b to w nastepnym wywolaniu wrocimy na poczatek *)
                a = if head_b = 0 then a1 + a2 else a1 + a3
            in if i = n then a else pom (i + 1) n b b2 a a1 a2 
        in pom 3 n (if tl (tl (tl b)) = [] then b else tl (tl (tl b))) b 1 1 0;; (* wywolanie poczatkowe z i = 3, stad jesli lista b ma 3 elemanty to zaczniemy od jej poczatku; z zalozenia zadania lista b ma co nanjmniej 3 elemaenty wiec wywolanie tl (tl (tl b)) nie zakonczy sie bledem *)

(* dowod poprawnosci 
funkcja miary f = n - i
i z kazdym wywolaniem rakurencji rosnie zatem f maleje do 0 wiec rekurencja sie skonczy
zakladajac ze i - 1 daje dobry wynik to wywoalnie dla i oblicza element x_i jako sume odpowiednich elementow ciagu zgodnie z trescia zadania
dla poczatkowego i = 3 funkcja zwraca poprawny wynik obliczony jako suma odpowiednich elementow ciagu zgodnie z trescia zadania
zatem rekurencja jest poprawna *)