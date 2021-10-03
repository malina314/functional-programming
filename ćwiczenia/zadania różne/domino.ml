open Array;;

let domino lst = 
  let tab = make_matrix 7 7 0 in
  let pom (a,b) = begin
    tab.(a).(b) <- tab.(a).(b) + 1;
    tab.(b).(a) <- tab.(b).(a) + 1;
  end in 
  List.iter pom lst;
  let wyn = ref [] in
  let dlugosc = ref 0 in
  let rec f a lista len = 
    if len > !dlugosc then begin
      dlugosc := len;
      wyn := lista;
    end;
      for i = 0 to 6 do
        let t = tab.(a).(i) in
        if t > 0 then begin
          tab.(a).(i) <- tab.(a).(i) - 1;
          tab.(i).(a) <- tab.(i).(a) - 1;
          f i ((a,i)::lista) (len + 1);          
          tab.(a).(i) <- tab.(a).(i) + 1;
          tab.(i).(a) <- tab.(i).(a) + 1;
        end
      done
    in
  for j = 0 to 6 do
      f j [] 0;    
    done;
  !wyn
  ;;
