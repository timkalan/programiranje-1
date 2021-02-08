(* a *)
let razlika_kvadratov x y = (x + y) * (x + y) - (x * x + y * y)


(* b *)
let uporabi_na_paru f (x1, y1) = (f x1, f y1)


(* c *)
let ponovi_seznam n sez = 
    if n <= 0 then []
    else 
        let rec aux acc pod = 
        match pod with
            | 0 -> acc
            | n -> aux (sez @ acc) (pod-1)
        in 
        aux [] n 


(* d *)
let razdeli list = 
    let rec aux neg poz = function 
        | [] -> (neg, poz)
        | x :: xs -> 
            if x < 0 then aux (x :: neg) poz xs
            else aux neg (x :: poz) xs
    in 
    aux [] [] list
