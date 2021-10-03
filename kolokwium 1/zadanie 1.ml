(* Mateusz Malinowski *)

open List;;

(* let a = [1;2;2;3;4;5;5;6;6;6;7];; *)

let wysun lst = (* lista z tresci zadania *)
    if lst = [] then []
    else
        let (wielokrotne, pojedyncze) =
            let rec pom lst x y e length_e = (* lst - pozostaly do rozpatrzenia ogon poczatkowej listy, x - odwrocona lista elementow wielokrotnych, y - odwrocona lista elementow pojedynczych, e - lista tymczasowa takich samych elementow, length_e - dlugosc listy e (bo List.length dziala liniowo wzgledem dlugosci a tak trzymajac jedna dodatkowa zmienna mamy w czasie stalym); funkcja zwraca pare list, pierwsza z nich zawiera elementy wielokrotne w kolejnosci odwrotnej do wejscia, druga zawiera elementy pojedyncze w kolejnosci odwrotnej do wejscia*)
                match lst with
                | [] -> if length_e > 1 then (e @ x, y) else (x, e @ y)
                | head :: tail when head = hd e -> pom tail x y (head :: e) (length_e + 1)
                | head :: tail -> if length_e > 1 then pom tail (e @ x) y [head] 1 else pom tail x (e @ y) [head] 1
            in pom (tl lst) [] [] [hd lst] 1
        in (rev wielokrotne) @ (rev pojedyncze);;

(* dowod poprawnosci 
funkcja miary f = length lst
i z kazdym wywolaniem rakurencji dlugosc lst maleje o 1 zatem f maleje do 0 wiec rekurencja sie skonczy
funkcja daje poprawny wynik, zakladajac ze poprzednie wywolanie tez dalo poprawny wynik, poniewaz sprawdza czy aktualnie rozpatrywany element jest taki sam jak poprzedni i jesli tak to dokleja go do tymczasowej listy takich samych elementów, w przeciwnym razie jesli bylo klika takich samych elementow to dokleja je do listy wielokrotnych elementow, w przeciwnym razie do pojedynczych i tworzy nowa liste tymczasowa
dla poczatkowego wywolania listy wynikowe sa puste a rozpatrywany element trafia na liste tymczasowa
zlozonosc jest liniowa poniewaz kazdy element zostanie rozpatrzony raz i trafi na liste tymczasowa ktora zostanie pewna liczbe razy doklejona do list wynikowych jednak kazy element poczatkowy bedzie doklejony do jednej z list wynikowych dokladnie raz, na koniec dokladnie raz obie listy wynikowe funkcji pomocniczej zostana odwrocone (co jeest liniowe) oraz zostana dokladnie raz sklejone (co jest liniowe wzgledem dlugosci listy wielokorotne*)