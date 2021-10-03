type expr = 
        And of expr * expr |
        Or of expr * expr |
        Not of expr | 
        Value of bool;;

let funkcja ex =
    let rec pom ex = (* (true, false) *)
        match ex with
        | Value (_) -> (1, 1)
        | Not (a) ->
            let (t, f) = pom a
            in (f, t)
        | Or (l, r) ->
            let ((lt, lf), (rt, rf)) = (pom l, pom r)
            in ((lt + lf) * (rt + rf) - lf * rf, lf * rf)
        | And (l, r) ->
            let ((lt, lf), (rt, rf)) = (pom l, pom r)
            in (lt * rt, (lt + lf) * (rt + rf) - lt * rt)
    in fst (pom ex);;