open List;;

type 'a tree =  Node of 'a * 'a tree list;;

let rec fold_tree f (Node (x, l)) = 
    f x (map (fold_tree f) l);;

let a = Node (1, [Node (2, []); Node (3, [Node (4, []); Node (5, [])]); Node (6, [])]);;

let print_lst lst =
    print_string "[";
    map (fun x -> print_int x; print_string "; ") lst;
    print_string "]; ";;

let print_lst_lst lst =
    print_string "[";
    map (print_lst) lst;
    print_string "]";
    print_newline ();;

let levels tree =
    let rec pom tree res_lst_lst =
        print_lst_lst res_lst_lst;
        let res_local = if res_lst_lst = [] then [[]] else res_lst_lst in
            match tree with
            | Node (v, []) ->
                (v :: hd res_local) :: (tl res_local)
            | Node (v, lst) ->
                (v :: hd res_local) :: (fold_left (fun acc el -> pom el acc) (tl res_local) lst)
    in pom tree [[]];;

levels a;;