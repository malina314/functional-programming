(* print_int xyz;; print_string "\n";; flush stdout;; let xyz = xyz + 1;; *)

let a = [1; 2; 3; 4; 5];;

let b = List.fold_left (fun _ x -> print_int x; print_newline ()) () a;;

let a = 3 and b = 5;;

let ptr = ref a;; (* int *ptr = &a; => ref <==> & *)
print_int (!ptr); print_newline;; (* !ptr <==> *ptr; *)
ptr := b;; (* ptr = &b *)
print_int (!ptr); print_newline;;
print_int (a); print_newline;;