open List;;

let wzrost lst =
    rev (let rec pom lst res reslen (chead :: ctail) currlen =
        match lst with
        | [] -> print_int 1; print_string "\n"; res
        | (head :: tail) when head > chead -> print_int 2; print_string "\n";
            if currlen >= reslen then
                pom tail (head :: chead :: ctail) (currlen + 1) (head :: chead :: ctail) (currlen + 1)
            else
                pom tail res reslen (head :: chead :: ctail) (currlen + 1)
        | (head :: tail) -> print_int 3; print_string "\n";
            if reslen = 0 then 
                pom tail [head] 1 [head] 1
            else
                pom tail res reslen [head] 1
    in pom lst [] 0 [max_int] 1);;

let a = [1;2;3;4;5];;
let b = [];;
let c = [1];;
let d = [max_int];;
let e = [min_int; max_int];;
let f = [0; -1; -2; -3];;
let g = [1; 2; 33; 1; 1; 1; 0; 4; 5; 6;];;
let h = [1; 2; 33; 55; 66; 77; 1; 1; 1; 0; 4; 5; 6; 0;];;


let wzrost2 lst =
    let x =
        let pom (res, reslen, (chead :: ctail), currlen) el =
            match el with
            | x when x > chead ->
                print_int 1; print_string "\n";
                if currlen >= reslen then
                    ((el :: chead :: ctail), (currlen + 1), (el :: chead :: ctail), (currlen + 1))
                else
                    (res, reslen, (el :: chead :: ctail), (currlen + 1))
            | _ -> print_int 2; print_string "\n";
                if reslen = 0 then 
                    ([el], 1, [el], 1)
                else
                    (res, reslen, [el], 1)
        in fold_left (pom) ([], 0, [max_int], 1) lst
    in match x with
    | (x, _, _, _) -> rev x;;
