let zdominowani graph =
    let n = Array.length graph in
    let low = Array.make n max_int in
    let visited = Array.make n false in
    let deep = Array.make n 0 in
    let rec find_low v d =
        visited.(v) <- true;
        deep.(v) <- d;
        low.(v) <- Array.fold_left (fun (a, i) x ->
            if
            if visited.(x) then
                min a deep.(x)
            else
                min a (find_low x (d + 1))
        ) d graph.(v);

        low.(v)
    in find_low 0 0;;






let g = [|
    [|false; true; true; false; false|];
    [|false; false; true; false; false|];
    [|false; false; true; false; false|];
    [|false; false; false; false; false|];
    [|false; true; false; false; false|];
|];;