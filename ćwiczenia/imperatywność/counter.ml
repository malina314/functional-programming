type counter = {
    mutable cnt : int;
    mutable epk : int;
}

let epoka = ref 0

let make () = {cnt = 0; epk = !epoka}

let inc c =
    if c.epk < !epoka then begin
        c.cnt <- 1; 
        c.epk <- !epoka;
        c.cnt
    end
    else begin
        c.cnt <- c.cnt + 1; 
        c.cnt
    end

(* let reset () = epoka := !epoka + 1;; *)
let reset () = incr epoka;;