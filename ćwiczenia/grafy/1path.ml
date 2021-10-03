type graph = int array array;;

let path (graph : graph) =
    let n = Array.length graph in
    let visited = Array.make n false in
    let result = Array.make n 0 in
    let rec dfs v =
        if visited.(v) then
            result.(v)
        else
            (visited.(v) <- true;
            let w = Array.fold_left (fun a x -> max a (if x > v then dfs x + 1 else 0)) 0 graph.(v) in
            result.(v) <- w;
            w)
    in
    let odp = ref 0 in
    for i = 0 to n - 1 do
        odp := max !odp (dfs i)
    done;
    !odp;;


let g = [|
    [|7|];           (* 0 *)
    [|2; 6; 7; 3|];      (* 1 *)
    [|5; 6; 7; 1|];      (* 2 *)
    [|1; 4|];     (* 3 *)
    [|1; 3; 5|];     (* 4 *)
    [|2; 3; 4|];     (* 5 *)
    [|1; 2|];     (* 6 *)
    [|0; 1; 2|]        (* 7 *)
|];;

path g;;