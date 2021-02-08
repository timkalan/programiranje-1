(* a *)
let podvoji_vsoto a b = 2 * (a + b)


(* b *)
let povsod_vecji (x1, y1, z1) (x2, y2, z2) =
    (x1 > x2) && (y1 > y2) && (z1 > z2)


(* c *)
let uporabi_ce_lahko f element = 
    match element with 
        | Some x -> Some (f x)
        | None -> None


(* d *)
let pojavi_dvakrat el l =  
    let rec aux acc = function
        | [] -> 0 
        | x :: xs -> if x = el then aux (1+acc) xs
            else aux acc xs
    in 
    (aux 0 l) = 2


(* e *)
let izracunaj_v_tocki  t f_list = 
    let rec aux acc = function 
        | [] -> acc
        | f :: fs -> aux ((f t) :: acc) fs
    in 
    aux [] f_list


(* f *)
let eksponent x p = 
    let rec aux acc podiramo =  
        match podiramo with
            | 0 -> acc
            | n -> aux (acc * x) (podiramo-1)
    in 
    aux 1 p