type 'a bin_tree = Node of 'a bin_tree * 'a * 'a bin_tree | Null;;

let rec fold_bin_tree f a t = 
    match t with
    | Null -> a
    | Node (l, x, r) -> f x (fold_bin_tree f a l) (fold_bin_tree f a r);;

let f _ (hl, sl) (hr, sr) = ((max hl hr + 1), (max (hl + hr) (max sl sr)));;

let srednica tree = snd (fold_bin_tree (f) (0, 0) tree);;

let a = Node (Null, 1, Null);;
let b = Node (a, 2, a);;
