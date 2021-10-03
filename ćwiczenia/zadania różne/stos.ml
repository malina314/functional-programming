open Stack;;

let s = create();;
push (max_int, 0) s;;

let katastrofy lst = 
    let f (l, i) x = 
        while fst (top s) < x do
            pop s
        done;
        let ile = i - snd (top s) in
        push (x, i) s;
        ((ile :: l), i + 1)
    in
    List.rev (fst (List.fold_left (f) ([], 1) lst));;


katastrofy [4; 3; 2; 1; 5; 1; 3];;