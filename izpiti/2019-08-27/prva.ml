let odstej_trojici tr1 tr2 = 
    let (a, b, c) = tr1 in 
    let (x, y, z) = tr2 in 
    (a - x, b - y, c - z)


let max_rezultat_do_n f n = 
    let rec aux max i = 
        if i = n + 1 then max 
        else if f i >= max then aux (f i) (i+1)
        else aux max (i+1)
    in 
    aux (f 0) 0 


let max_rezultat_do_n2 f n =
    let rec aux trenutni_max trenutni_i = 
        if trenutni_i < 0 then trenutni_max 
        else (
            let v = f trenutni_i in 
            aux (max v trenutni_max) (trenutni_i)
        )
    in 
    aux (f n) (n - 1)


let rec pocisti_seznam_bedna = function 
    | [] -> []
    | (Some x) :: xs -> x :: pocisti_seznam_bedna xs
    | _ :: xs -> pocisti_seznam_bedna xs


let pocisti_seznam sez = 
    let rec aux gradimo podiramo = 
        match podiramo with
        | [] -> List.rev gradimo 
        | x :: xs ->
            match x with
            | Some y -> aux (y :: gradimo) xs
            | None -> aux gradimo xs
    in 
    aux [] sez


let preveri_urejenost sez = 
    let rec aux lihi sodi podiramo = 
        match podiramo with
        | [] -> (sodi = List.sort compare sodi) && (List.rev lihi = List.sort compare lihi)
        | x :: xs -> if x mod 2 = 0 then aux lihi (sodi @ [x]) xs 
                     else aux (lihi @ [x]) sodi xs 
    in 
    aux [] [] sez


let preveri_urejenost2 l = 
    let rec aux min_sodo max_liho = function
        | [] -> true 
        | x :: xs -> if x mod 2 = 0 then (x > min_sodo) && (aux x max_liho xs)
                     else x < (max_liho) && (aux min_sodo x xs)
    in 
    aux min_int max_int l