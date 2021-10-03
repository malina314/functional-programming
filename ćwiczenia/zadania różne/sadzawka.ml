(* w kazdym kroku wkladamy najwyzej jeden element, kazdy wlozony element zostanie zdjety najwyzej raz -> O(n) *)

let sadzawka temp_lst = 
    let rec pom temp_lst stan =
        match temp_lst with
        | [] -> stan
        | (temp_dzisiaj :: tail) ->
            pom tail (
            if temp_dzisiaj > 0 then
                let rec pom2 warstwy temp woda = (* rozmarzanie *)
                    match warstwy with
                    | [] -> []
                    | (h :: t) ->
                        if h < 0 then
                            if temp >= -h then pom2 t (temp + h) (woda - h)
                            else (woda + temp) :: (h + temp) :: t
                        else
                            pom2 t temp (woda + h)
                in pom2 stan temp_dzisiaj 0
            else if temp_dzisiaj < 0 then
                let rec pom2 warstwy temp lod = (* zamarzanie *)
                    match warstwy with
                    | [] -> [lod + temp]
                    | (h :: t) ->
                        if h > 0 then
                            if -temp >= h then pom2 t (temp + h) (lod - h)
                            else (lod + temp) :: (h + temp) :: t
                        else
                            pom2 t temp (lod + h)
                in pom2 stan temp_dzisiaj 0
            else
                stan)
    in pom temp_lst [];;

let a = [2; -12; 8; -4; 2; 4; -4; 1; -3];;

sadzawka a;;