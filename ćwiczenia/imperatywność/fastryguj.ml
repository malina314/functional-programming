type 'a tree =
        Node of 'a * 'a tree * 'a tree * 'a tree ref |
        Null;;

open List;;

let fastryguj tree =
    let rec inorder t a =
        match t with
        | Null -> a
        | Node (_, l, r, _) -> inorder l (t :: (inorder r a))
    in 
    let lst = inorder tree [Null] in
    let f (Node (_, _, _, rr)) x =
        rr := x;
        x
    in
    fold_left (f) (hd lst) (tl lst);
    hd lst;;


let test_tree = Node(42, Node(24, Null, Node(7, Node(8, Null, Null, ref Null), Null, ref Null), ref Null), Node(12, Node(3, Null, Node(2, Null, Null, ref Null), ref Null), Node(5, Null, Null, ref Null), ref Null), ref Null);;
