let silnie n =
    let rec pom n lst =
        if n = 0 then lst else
            let (x, k) = 
                let rec znajdz_silnie x n k = 
                    if n > x then (n / (k - 1), k - 2)
                    else znajdz_silnie x (n * k) (k + 1)
                in znajdz_silnie n 1 2
            in pom (n - x) (k :: lst)
    in pom n [];;
