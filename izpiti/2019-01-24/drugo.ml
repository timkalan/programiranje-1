(* a *)
type 'a mm_drevo =
    | Prazno 
    | Vozlisce of 'a * int * 'a mm_drevo * 'a mm_drevo


(* b *)
let rec vstavi drevo a = 
    match drevo with 
        | Prazno -> Vozlisce (a, 1, Prazno, Prazno)
        | Vozlisce (x, st, t1, t2) -> 
            if x = a then Vozlisce (x, st+1, t1, t2)
            else if (a < x) then Vozlisce (x, st, vstavi t1 a, t2)
            else Vozlisce (x, st, t1, vstavi t2 a)


(* c *)
let multimnozica_iz_seznama sez = 
    let rec aux acc = function 
        | [] -> acc
        | x :: xs -> aux (vstavi acc x) xs
    in 
    aux Prazno sez


(* d *)
let rec velikost_multimnozice = function 
    | Prazno -> 0 
    | Vozlisce (_, st, t1, t2) -> 
        st + velikost_multimnozice t1 + velikost_multimnozice t2


(* e *)
let seznam_iz_multimnozice drevo = 
    let rec iz_vozlisca x st acc =
        match st with 
            | 0 -> acc
            | n ->  iz_vozlisca x (st-1) (x :: acc)
    in

    let rec aux (acc : 'a list) = function 
        | Prazno -> acc
        | Vozlisce (x, st, t1, t2) ->
            (aux ((iz_vozlisca x st []) :: acc) t1) @ 