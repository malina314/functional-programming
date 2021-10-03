let rec shuffle lista1 lista2 =
  match (lista1, lista2) with
    ([], []) -> [] |
    ([], head :: tail) -> head :: shuffle tail [] |
    (head :: tail, []) -> head :: shuffle tail [] |
    (head1 :: tail1, head2 :: tail2) -> head1 :: head2 :: shuffle tail1 tail2;;

    
shuffle [1;2] [3;4];;