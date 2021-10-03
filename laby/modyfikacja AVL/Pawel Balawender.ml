(* projekt zaliczeniowy "modyfikacja drzew" na WPF 2020 @ MIMUW
 * autor: Pawel Balawender
 * para do code review: Mateusz Malinowski, gr.
 * data: listopad 2020
 *)
 
(* int interval *)
type interv = int * int
 
(* interval set: Empty | (left, value, right, height, n_below *)
type iset =
  | Empty
  | Node of iset * interv * iset * int * int
 
(* iset tree together with its interval comparison function
 * member of the interface *)
type t = {
  cmp : interv -> interv -> int;
  set : iset;
}
 
(* ------------------------------------------------------------------------- *)
(* t constructor *)
let empty = { cmp = compare; set = Empty }
 
(* t selector *)
let is_empty (x: t) =
  x.set = Empty
 
(* sum without overflow *)
let isum (a: int) (b: int) =
  if a >= 0 && b >= 0 && a >= max_int - b
    then max_int
  else if a <= 0 && b <= 0 && a <= min_int - b
    then min_int
  else
    a + b
 
(* interval comparison function
 * it treats overlapping intervals as equal, which need special handling *)
let compare (a: interv) (b: interv) =
  let (amin, amax) = a
  and (bmin, bmax) = b in
  if amax < bmin
    then -1
  else if
    bmax < amin then 1
  else
    0
 
(* get tree height *)
let height (iset: iset) =
  match iset with
  | Node (_, _, _, h, _) -> h
  | Empty -> 0
 
(* get below *)
let below (iset: iset) =
  match iset with
  | Node (_, _, _, _, bel) -> bel
  | Empty -> 0
 
(* root interval length *)
let length (iset: iset) =
  match iset with
  | Node (_, (root_start, root_end), _, _, _) -> root_end - root_start + 1
  | Empty -> 0
 
(* contruct new tree from subtrees and the root interval *)
let make (left: iset) (value: interv) (right: iset) =
  let new_height = max (height left) (height right) + 1
  and below_left = isum (below left) (length left)
  and below_right = isum (below right) (length right) in
  let new_below = isum below_left below_right in
  Node (left, value, right, new_height, new_below)
 
(* tree balancer *)
let bal (l: iset) (k: interv) (r: iset) =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else
    make l k r
 
(* get minimal element *)
let rec min_elt (iset: iset) =
  match iset with
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found
 
(* remove minimal element *)
let rec remove_min_elt (iset: iset) =
  match iset with
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "PSet.remove_min_elt"
 
(* merge two trees *)
let merge (t1: iset) (t2: iset) =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _, _ ->
      let k = min_elt t2 in
      bal t1 k (remove_min_elt t2)
 
(* add an interval to a tree; interval has to be disjoint with
 * the tree's intervals *)
let rec add_one_disj cmp (x: interv) (iset: iset) =
  match iset with
  | Node (l, k, r, h, bel) ->
      let c = cmp x k in
      if c < 0 then
        let nl = add_one_disj cmp x l in
        bal nl k r
      else if c > 0 then
        let nr = add_one_disj cmp x r in
        bal l k nr
      else
        invalid_arg "should be disjoint"
  | Empty -> Node (Empty, x, Empty, 1, 0)
 
(* alias *)
let add_one = add_one_disj
 
(* join two trees to a common root *)
let rec join cmp (l: iset) (v: interv) (r: iset) =
  match (l, r) with
    (Empty, _) -> add_one cmp v r
  | (_, Empty) -> add_one cmp v l
  | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
      if lh > rh + 2 then bal ll lv (join cmp lr v r) else
      if rh > lh + 2 then bal (join cmp l v rl) rv rr else
      make l v r
 
(* split tree to its subtrees with elements lower and greater than x *)
let split (x: int) {cmp=cmp; set=set} =
  let rec loop (x: int) (set: iset) =
    match set with
    | Empty ->
        (Empty, false, Empty)
    | Node (l, ((vstart, vend) as v), r, _, _) ->
        let c = cmp (x, x) (vstart, vend) in
        if c = 0 then
  let new_left =
    if x > vstart then add_one cmp (vstart, x-1) l
    else l
  and new_right =
    if x < vend then add_one cmp (x+1, vend) r
    else r
  in
  (new_left, true, new_right)
        else if c < 0 then
          let (ll, pres, rl) = loop x l in
          (ll, pres, join cmp rl v r)
        else
          let (lr, pres, rr) = loop x r in
          (join cmp l v lr, pres, rr)
  in
  let setl, pres, setr = loop x set in
  ({cmp=cmp; set=setl}, pres, {cmp=cmp; set=setr})
 
(* remove interval from tree *)
let remove (x: interv) (tree: t) =
  let xstart, xend = x in
  let (new_left, _, _) = split xstart tree
  and (_, _, new_right) = split xend tree
  in merge new_left.set new_right.set
 
(* add interval to a tree *)
let add (x: interv) (tree: t) =
  {cmp = tree.cmp; set = add_one tree.cmp x (remove x tree)}
 
(* check if tree contains an integer *)
let mem (x: int) (tree: t) =
  let rec loop = function
    | Node (left, root, right, _, _) ->
        let c = tree.cmp (x, x) root in
        c = 0 || loop (if c < 0 then left else right)
    | Empty -> false
  in
  loop tree.set
 
(* apply f to all intervals of tree *)
let iter f (tree: t) =
  let rec loop = function
    | Node (left, root, right, _, _) ->
      loop left; f root; loop right
    | Empty -> ()
  in
  loop tree.set
 
(* accumulate f along tree *)
let fold f (tree: t) acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (left, root, right, _, _) ->
          loop (f root (loop acc left)) right
  in
  loop acc tree.set
 
(* convert tree to a sorted list of its nodes *)
let elements (tree: t) =
  let rec loop acc = function
    | Node(l, k, r, _, _) -> loop (k :: loop acc r) l
    | Empty -> acc
  in
  loop [] tree.set