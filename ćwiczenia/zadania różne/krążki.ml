open List;;

let rec lepsza_studnia x acc at =
  if length x > 0 then
    if length acc = 0 then
      lepsza_studnia (tl x) [hd x] (hd x)
    else
      if (hd x) > at then
        lepsza_studnia (tl x) (acc @ [hd x]) (hd x)
      else
        lepsza_studnia (tl x) acc at
  else
    acc;;

let studnia x =
  lepsza_studnia x [] 0;;

studnia [1;2;4;2;5;2;8;1;2;9];;
