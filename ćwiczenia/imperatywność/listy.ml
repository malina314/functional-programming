type 'a option = None | Some of 'a

type 'a elem = {v: 'a; mutable next: 'a lista}
and  'a lista = 'a elem option

let get (Some (x)) = x;;

let przeplot a b =
    let rw = ref a in
    let w = !rw in
    let ra = ref (get a).next and rb = ref b in
    while !ra <> None && !rb <> None do
        let an = (get !ra).next and bn = (get !rb).next in
        (if (get !ra).v <= (get !rb).v then
            ((get !rw).next <- !ra;
            rw := (get !rw).next;
            ra := an;
            (* print_string "A " *)
            )
        else
            ((get !rw).next <- !rb;
            rw := (get !rw).next;
            rb := bn;
            (* print_string "B " *)
            ));
        (* print_int (get !ra).v; print_string " "; *)
        (* print_int (get !rb).v; print_string "\n"; *)
        (* read_int (); *)
    done;
    (if !ra <> None then
        (get !rw).next <- !ra
    else
        (get !rw).next <- !rb);
    w;;

(* let a = [1; 2; 4; 5; 9; 11];;
let b = [1; 3; 5];; *)
let a = Some ({v = 1; next = Some ({v = 2; next = Some ({v = 4; next = Some ({v = 5; next = Some ({v = 9; next = Some ({v = 11; next = None})})})})})});;
let b = Some ({v = 1; next = Some ({v = 3; next = Some ({v = 5; next = None})})});;

przeplot a b;;

(* assert (false);; *)