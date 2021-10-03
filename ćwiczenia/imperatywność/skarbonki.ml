let rec dfs lst vis ind x = 
    if vis.(ind) = x then 1
    else if vis.(ind) <> 1 then 0
    else begin 
        vis.(ind) <- x; 
        dfs lst vis lst.(ind) x;
    end 

let skarbonki lst =
    let w = ref 0 in
    let n = Array.length lst in
    let vis = Array.make n (-1) in
    for i = 0 to 1 do 
        if vis.(1) = -1 then 
            w := !w + dfs lst vis i i;
    done;
    !w;; 

let a = [|1;2;3|];;
let b = [|11;15;28;19;26;8;17;8;29;16;12;8;19;5;6;6;3;16;12;11;15;28;17;24;3;14;30;30;9;22|];;