(* autor: Mateusz Malinowski *)
(* reviewer: Mikołaj Uzarski gr. 5 *)

open Array;;

let przelewanka input_array =
    let lst = fold_right (fun x a -> if fst x <> 0 then x :: a else a) input_array [] in (* usuwa szklanki o pojemnosci 0 *)
    let improved_array = of_list lst in
    let test = fold_left (fun a (x, y) -> if y = 0 || y = x then true else a) false improved_array in (* na koncu musi zostac przynajmniej jedna szklanka pusta lub pelna, inaczej sie nie da *)
    if improved_array = [||] then
        0
    else if test then
        let exception Solved of int in (* jezeli znajdziemy rozwiazanie to mozemy przerwac obliczenia i zwrocic wynik za pomoca wyjatku *)
        let bfs () = (* przeszukiwanie BFS grafu stanow *)
            let l = length improved_array in
            let pojemnosc = make l 0 in
            let stan_koncowy = make l 0 in
            iteri (fun i (x, y) -> stan_koncowy.(i) <- y; pojemnosc.(i) <- x) improved_array;
            let rec gcd a b = if a = 0 then b else gcd (b mod a) a in
            let nwd = fold_left gcd 0 pojemnosc in
            let test2 = fold_left (fun a x -> if x mod nwd <> 0 then false else a) true stan_koncowy in
            if test2 = false then begin (* nwd pojemnosci musi dzielic wszystkie wartosci stan_koncowy, inaczej sie nie da *)
                raise (Solved (-1))
            end;
            let stan = make l 0 in (* poczatkowo wszystkie szklanki sa puste *)
            if stan = stan_koncowy then begin
                raise (Solved 0)
            end;
            let vis = Hashtbl.create 1 in (* zbior rozpatrzonych stanow *)
            Hashtbl.add vis (copy stan) 0;
            let q = Queue.create () in
            Queue.add ((copy stan), 0) q;
            while not (Queue.is_empty q) do
                let (stan, ile) = Queue.pop q in (* ile - ilosc ruchow potrzebnych do uzysaknia stanu (dlugosc krawedziowa sciezki w grafie) *)
                for i = 0 to l - 1 do (* nalewanie *)
                    if stan.(i) < pojemnosc.(i) then begin
                        let buf = stan.(i) in
                        stan.(i) <- pojemnosc.(i);
                        if stan = stan_koncowy then begin
                            raise (Solved (ile + 1))
                        end;
                        if Hashtbl.mem vis stan = false then begin
                            Hashtbl.add vis (copy stan) 0;
                            Queue.add ((copy stan), ile + 1) q;
                        end;
                        stan.(i) <- buf;
                    end;
                done;
                for i = 0 to l - 1 do (* wylewanie *)
                    if stan.(i) > 0 then begin
                        let buf = stan.(i) in
                        stan.(i) <- 0;
                        if stan = stan_koncowy then begin
                            raise (Solved (ile + 1))
                        end;
                        if Hashtbl.mem vis stan = false then begin
                            Hashtbl.add vis (copy stan) 0;
                            Queue.add ((copy stan), ile + 1) q;
                        end;
                        stan.(i) <- buf;
                    end;
                done;
                for i = 0 to l - 1 do (* przelewanie z i do j *)
                    for j = 0 to l - 1 do
                        if i <> j && stan.(j) < pojemnosc.(j) && stan.(i) > 0 then begin
                            let buf_i = stan.(i) in
                            let buf_j = stan.(j) in
                            if pojemnosc.(j) - stan.(j) >= stan.(i) then begin
                                stan.(j) <- stan.(i) + stan.(j);
                                stan.(i) <- 0;
                            end
                            else begin
                                stan.(i) <- stan.(i) - pojemnosc.(j) + stan.(j);
                                stan.(j) <- pojemnosc.(j);
                            end;
                            if stan = stan_koncowy then begin
                                raise (Solved (ile + 1))
                            end;
                            if Hashtbl.mem vis stan = false then begin
                                Hashtbl.add vis (copy stan) 0;
                                Queue.add ((copy stan), ile + 1) q;
                            end;
                            stan.(i) <- buf_i;
                            stan.(j) <- buf_j;
                        end
                    done;
                done;
            done;
            -1
        in
        try bfs ()
        with Solved x -> x
    else
        -1;;
