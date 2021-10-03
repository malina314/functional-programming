let kijek lst =
    let l = List.length lst in
    let m = Array.make_matrix l l max_int in
    let pref = Array.make (l + 1) 0 in
    let get_sum p k = pref.(k + 1) - pref.(p) in
    List.iteri (fun i x -> pref.(i + 1) <- pref.(i) + x) lst;
    for i = 0 to l - 1 do
        m.(i).(i) <- 0
    done;
    for i = 1 to l - 1 do
        for a=0 to l - i - 1 do
            let b = a + i in
            for c = a to b - 1 do
                m.(a).(b) <- min (m.(a).(c) 
                    + m.(c + 1).(b) + max (get_sum a c) (get_sum (c + 1) b)) (m.(a).(b))
            done
        done
    done;
    if l = 0 then 
        0 
    else 
        m.(0).(l - 1);;