open Array;;

let wydaj k nominaly =
    let tab = make (k + 1) (k + 1) in
    tab.(0) <- 0;
    let pom n =
        for i = n to k do
            tab.(i) <- min tab.(i) (tab.(i - n) + 1)
        done
    in
    List.iter pom nominaly;
    if tab.(k) > k then
        failwith "Nie da sie"
    else
        tab.(k);;