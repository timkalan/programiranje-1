(* a *)
type filter_tree = 
    | Vozlisce of filter_tree * int * filter_tree
    | List of int list

let primer = Vozlisce (
    Vozlisce (
        List ([1]), 
        5, 
        List ([])
    ), 
    10, 
    Vozlisce (
        List ([]), 
        15, 
        List ([19; 20])
    )
)


(* b *)
let rec vstavi st drevo = 
    match drevo with 
        | List x -> List (st :: x)
        | Vozlisce (t1, x, t2) ->
            if st <= x then Vozlisce (vstavi st t1, x, t2)
            else Vozlisce (t1, x, vstavi st t2)


(* c *)
let vstavi_seznam sez drevo = 
    let rec aux grad = function 
        | [] -> grad
        | x :: xs -> aux (vstavi x drevo) xs
    in 
    aux drevo sez


(* d *)
let rec preveri drevo = 
    match drevo with
    | Vozlisce (List x, t, List y) -> ()