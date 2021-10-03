(* autor: Mikołaj Uzarski *)

open Array;;

(* wyjatek rzucany przy znalazieniu wyniku *)
exception Found;;

let przelewanka gl =

  let res = ref (-1) in

  if gl = [||] then 0 else

  let len = length gl in

  let rec nwd a b =
    if b = 0 then a
    else nwd b (a mod b)
  in

  (* sprawdzamy czy nwd objetosci dzieli kazda z liczb ze stanu koncowego *)
  let check_nwd =
      let nwd_ = fold_left (fun acc (x, _) -> nwd acc x) 0 gl in
      if nwd_ = 0 then true else
        fold_left (fun acc (_, y) -> acc && y mod nwd_ = 0) true gl
  in

  (* warunkiem koniecznym istnienia rozwiazania, jest to aby w koncowej konfiguracji *)
  (* co najmniej jedna szklanka byla pusta lub pelna *)
  let check_final =
    let count = ref 0 in
      for i = 0 to len - 1 do
        if (snd gl.(i)) = 0 || (snd gl.(i)) = (fst gl.(i)) then
          count := !count + 1;
      done; if !count = 0 then false else true
  in

  (* sprawdzanie warunkow koniecznych *)
  let check = check_nwd && check_final in

  (* stan po napelnieniu gl s *)
  let fill s state =
    let res = copy state in
      res.(s) <- fst gl.(s); res
  in

  (* stan po wylaniu ze gl s *)
  let spill s state =
    let res = copy state in
      res.(s) <- 0; res
  in

  (* dostaniemy liste stanow, po przelaniu wody ze gl s do kazdej innej *)
  let transfer s state =
    let res = ref [] in begin
      for i = 0 to len - 1 do
        if i <> s then begin
          let cp = copy state in
          if (fst gl.(i)) - cp.(i) < cp.(s) then begin
            cp.(s) <- cp.(s) - ((fst gl.(i)) - cp.(i));
            cp.(i) <- fst gl.(i);
            res := cp :: !res;
          end else begin
            cp.(i) <- cp.(i) + cp.(s);
            cp.(s) <- 0;
            res := cp :: !res;
          end
        end
      done
    end; !res
  in

  (* koncowy stan, do ktorego dazymy *)
  let final_state =
    let tmp = make len 0 in begin
      for i = 0 to len - 1 do
        tmp.(i) <- snd gl.(i);
      done
    end; tmp
  in

  (* sprawdzamy, czy stan state, otrzymany w kroku step jest równy docelowemu stanowi *)
  let test_final_state state step =
    if state = final_state then begin
      res := step + 1;
      raise Found;
    end
  in
  (* bfs po stanach *)
  (* dla danego stanu cur robimy: *)
  (* - tworzymy stany, powstale przez wykonanie wszystkich operacji na szklankach ze stanu cur *)
  (* - sprawdzamy czy któryś ze stanów, jest stanem szukanym *)
  (* - dodajemy je do kolejki FIFO i tablicy haszujacej, o ile nie rozpatrywalismy wczesniej danego stanu *)
  let bfs () =
    let state = make len 0 in
    let q = Queue.create () in
    let visited = Hashtbl.create 1 in begin
      Queue.push (state, 0) q;
      Hashtbl.add visited state 0;
      test_final_state state (-1);
      while not (Queue.is_empty q) do
        let (cur, step) = Queue.pop q in begin
          for i = 0 to len - 1 do
            let s_fill = fill i cur in
            let s_spill = spill i cur in
            let s_transfer = transfer i cur in begin
              if not (Hashtbl.mem visited s_fill) then begin
                test_final_state s_fill step;
                Queue.push (s_fill, step + 1) q;
                Hashtbl.add visited s_fill 0;
              end;
              if not (Hashtbl.mem visited s_spill) then begin
                test_final_state s_spill step;
                Queue.push (s_spill, step + 1) q;
                Hashtbl.add visited s_spill 0;
              end;
              List.iter (fun x ->
                if not (Hashtbl.mem visited x) then begin
                  test_final_state x step;
                  Queue.push (x, step + 1) q;
                  Hashtbl.add visited x 0;
                end) s_transfer;
            end
          done
        end
      done
    end; !res
  in

  if not check then (-1) else try (bfs ()) with Found -> !res;;