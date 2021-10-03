let rec czyRzadkie n = 
  if n < 2 then
    1
  else
    if n mod 2 = 1 && n / 2 mod 2 = 1 then
      0
    else
      czyRzadkie (n / 2);;

let rec rzadkie n = 
  if n = 1 then
    1
  else
    czyRzadkie n + rzadkie (n - 1);;