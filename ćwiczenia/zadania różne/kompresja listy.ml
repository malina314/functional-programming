open List;;

let rec push x (co, ile) =
    if ile > 0 then push (x @ [co]) (co, ile - 1)
    else x;;

let f x =
    let rec pom x acc =
        if x mod 2 = 0 then pom (x / 2) (acc + 1)
        else acc
    in pom x 0;;

let konwertuj x =
    let ile = f x
    in ((x / int_of_float (2. ** float_of_int ile) + 1) / 2, ile + 1)

let dekompresuj x =
    let rec dekompresuj_pom x res =
        if length x > 0 then dekompresuj_pom (tl x) (push res (konwertuj (hd x)))
        else res
    in dekompresuj_pom x [];;

dekompresuj [1; 6; 9; 42; 3];;

let pot n =
    let rec pot n w =
        if n = 1 then w
        else pot (n - 1) (w * 2)
    in pot n 1;;

let rec print_list = function 
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l

let kompresuj x =
    let rec pom x a =
        match x with
        | [] -> a
        | head_x :: tail_x ->
            let (ile, tail2) =
                let rec licz x2 n h =
                    print_list x2;
                    print_string "\n";
                    match x2 with
                    | [] -> (n, [])
                    | hdd :: t ->
                        if hdd = h then
                            licz t (n + 1) h
                        else
                            (n, x2)
                in licz tail_x 1 head_x
            in pom tail2 ([pot (ile - 1) * (2 * head_x - 1)] :: a)
    in pom x [];;


kompresuj [2;2;2;3;3;4;5;1;1;1];;
