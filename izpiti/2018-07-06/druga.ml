type 'a neprazen_sez = 
    | Konec of 'a 
    | Sestavljen of 'a * 'a neprazen_sez

(* a *)
let prvi = function 
    | Konec x -> x
    | Sestavljen (x, sez) -> x

let rec zadnji = function
    | Konec x -> x
    | Sestavljen (x, sez) -> zadnji sez


(* b *)
let rec dolzina = function 
    | Konec _ -> 1
    | Sestavljen (_, sez) -> 1 + dolzina sez


(* c *)
let pretvori_v_seznam seznam = 
    let rec aux acc = function 
        | Konec x -> x :: acc
        | Sestavljen (x, sez) -> aux (x :: acc) sez
    in 
    aux [] seznam


(* d *)
let rec zlozi f b = function 
    | Konec (y) -> f y b
    | Sestavljen (x, Sestavljen (y, rep)) -> zlozi f b (Sestavljen ((f x y), rep))
    | _ -> failwith "pls ne"