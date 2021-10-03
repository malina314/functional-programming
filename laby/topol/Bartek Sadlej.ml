(* autor : Bartek Sadlej *)
(* code review :  *)



(* 
################################
#      zmodyfikowana mapa      #
#        zmienione add         #
################################
*)

type ('k, 'v) map =
  | Empty
  | Node of ('k, 'v) map * 'k * 'v * ('k, 'v) map * int

type ('k, 'v) t =
  {
    cmp : 'k -> 'k -> int;
    map : ('k, 'v) map;
  }

let height = function
  | Node (_, _, _, _, h) -> h
  | Empty -> 0

let make l k v r = Node (l, k, v, r, max (height l) (height r) + 1)

let bal l k v r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lv, lr, _) ->
        if height ll >= height lr then make ll lk lv (make lr k v r)
        else
          (match lr with
          | Node (lrl, lrk, lrv, lrr, _) ->
              make (make ll lk lv lrl) lrk lrv (make lrr k v r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rv, rr, _) ->
        if height rr >= height rl then make (make l k v rl) rk rv rr
        else
          (match rl with
          | Node (rll, rlk, rlv, rlr, _) ->
              make (make l k v rll) rlk rlv (make rlr rk rv rr)
          | Empty -> assert false)
    | Empty -> assert false
  else Node (l, k, v, r, max hl hr + 1)

let rec min_binding = function
  | Node (Empty, k, v, _, _) -> k, v
  | Node (l, _, _, _, _) -> min_binding l
  | Empty -> raise Not_found

let rec remove_min_binding = function
  | Node (Empty, _, _, r, _) -> r
  | Node (l, k, v, r, _) -> bal (remove_min_binding l) k v r
  | Empty -> invalid_arg "PMap.remove_min_binding"

let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k, v = min_binding t2 in
      bal t1 k v (remove_min_binding t2)

let create cmp = { cmp = cmp; map = Empty }
let empty = { cmp = compare; map = Empty }

let is_empty x = 
	x.map = Empty

(* ## tu zmiany ## *)
(* zmienione na dopisywanie do listy, lub jednoelementowa lista dla pierwszego elementu, lub pusta lista dla pustej listy *)
let add x d { cmp = cmp; map = map } =
  let rec loop = function
    | Node (l, k, v, r, h) ->
        let c = cmp x k in
        if c = 0 then Node (l, x, d::v, r, h)
        else if c < 0 then
          let nl = loop l in
          bal nl k v r
        else
          let nr = loop r in
          bal l k v nr
    | Empty -> Node (Empty, x, [d], Empty, 1) in
  { cmp = cmp; map = loop map }
(* ## koniec zmian ## *)

let find x { cmp = cmp; map = map } =
  let rec loop = function
    | Node (l, k, v, r, _) ->
        let c = cmp x k in
        if c < 0 then loop l
        else if c > 0 then loop r
        else v
    | Empty -> raise Not_found in
  loop map

let remove x { cmp = cmp; map = map } =
  let rec loop = function
    | Node (l, k, v, r, _) ->
        let c = cmp x k in
        if c = 0 then merge l r else
        if c < 0 then bal (loop l) k v r else bal l k v (loop r)
    | Empty -> Empty in
  { cmp = cmp; map = loop map }

let mem x { cmp = cmp; map = map } =
  let rec loop = function
    | Node (l, k, v, r, _) ->
        let c = cmp x k in
        c = 0 || loop (if c < 0 then l else r)
    | Empty -> false in
  loop map

let exists = mem

let iter f { map = map } =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, v, r, _) -> loop l; f k v; loop r in
  loop map

let map f { cmp = cmp; map = map } =
  let rec loop = function
    | Empty -> Empty
    | Node (l, k, v, r, h) -> 
	  let l = loop l in
	  let r = loop r in
	  Node (l, k, f v, r, h) in
  { cmp = cmp; map = loop map }

let mapi f { cmp = cmp; map = map } =
  let rec loop = function
    | Empty -> Empty
    | Node (l, k, v, r, h) ->
	  let l = loop l in
	  let r = loop r in
	  Node (l, k, f k v, r, h) in
  { cmp = cmp; map = loop map }

let fold f { cmp = cmp; map = map } acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, v, r, _) ->
	  loop (f v (loop acc l)) r in
  loop acc map

let foldi f { cmp = cmp; map = map } acc =
  let rec loop acc = function
    | Empty -> acc
	| Node (l, k, v, r, _) ->
       loop (f k v (loop acc l)) r in
  loop acc map

(*
###################
#   koniec mapy   #
###################
*)





(* 
implementacja topol.mli
*)

exception Cykliczne;;


let topol (adjacency_list : ('a * 'a list) list) =
    (* dodawanie następników do mapy *)
    let adjacency_map = List.fold_left (
        fun part_result (this, neighbours) ->
            List.fold_left(
                fun temp_acc neighbour ->
                    add this neighbour temp_acc
            )
            part_result
            neighbours
    )
    empty
    adjacency_list in
    
    (* przypisywanie każdej czynności jej kolejności od końca, potem wynik jest zamieniany ( liczba_czynnosci - wynik_dla_danej ) na właściwą kolejność *)
    (* działa na zasadzie przechodzenia drzewa DFS, jak jakaś czynność nie ma następnika to przypisujemy jej moment t i zwracamy t + 1 *)
    (* w przeciwnym wypadku funkcja wywołuje się rekurencyjnie i czeka na powrót rekurencji z wynikiem dla danej czynności*)

    (* sprawdzenie czy graf opisujący kolejność czynności nie jest cykliczny polega na trzumaniu mapy aktualnie odwedzonych *)
    (* jeśli dana czynnośc była już odwiedzona ale nie ma dla niej wyniku to znaczy że znajduje się w cyklu, wtedy funkcja podnosi wyjątek Cykliczne *)
    let rec time_setter results time visited event=
        
        if not (mem event results) then

            if mem event visited then
                raise Cykliczne
            else

            let (updated_results, updated_time, updated_visited) =

            (* jeśli dana czynnośc nie ma żadnego następnika, nie jest zapisana w mapie sąsiedstwa więc trzeba rozpatrzeć wyjątek zwrócony przez find *)
            try 
                let next_events = find event adjacency_map in

                 List.fold_left 
                    
                    (fun (updated_results , updated_time , updated_visited) next_event ->
                        time_setter 
                            updated_results 
                            updated_time 
                            updated_visited 
                            next_event) 

                    (results,time,add event 1 visited) 

                    next_events
            with
                Not_found -> (results, time, add event 1 visited) in
                
            (add event updated_time updated_results, updated_time + 1, updated_visited)

        else
            (results,time,visited) in
    
    (* result_tem to wynik w odwróconej koleności *)
    (* time_end to liczba wszystkich czynności, potrzebne do odwrócenia kolejności *)
    let (result_temp, time_end, _) =
     
    List.fold_left 
        
        (fun (results, time, visited) (event, _) ->
            time_setter 
                results 
                time 
                visited 
                event) 

        (empty, 1, empty)

        adjacency_list in
    
    (* odwracanie numeracji result_temp*)
    (* potem sortujemy od pierwszej czynności do ostatniej i zwracamy wynik liste czynności (czyli drugi element z każdej pray) *)
    (foldi 
        (fun event time acc -> (time_end - (List.hd time), event)::acc)
        result_temp
        []
    ) |> List.sort compare |> List.map snd;;

