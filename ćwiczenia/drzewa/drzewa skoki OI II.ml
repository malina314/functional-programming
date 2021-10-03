type 'a tree = Node of ('a * 'a tree list) 

let obejscie drzewo = 
    let rec pom (Node(v,lst)) acc parz =
        if parz then List.fold_left (fun ace node -> pom node acc (not parz)) (v::acc) Ist
        else v::(List.fold_left (fun ace node -> pom node acc (not parz)) acc Ist) in pom drzewo [] false 

