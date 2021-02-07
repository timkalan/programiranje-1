type 'a kuhinjski_element =
    | Ponev of 'a
    | Lonec of 'a * 'a
    | Omara of 'a list


(* a *)
let kuhinja = [
    Ponev("tuna");
    Lonec("brokoli", "mango");
    Omara(["sir"; "toast"; "sok"; "ragu"])]


(* b *)
let rec prestej = function 
    | [] -> 0
    | x :: xs ->
        match x with
            | Ponev(_) -> 1 + prestej xs
            | Lonec(_) -> 2 + prestej xs
            | Omara(list) -> (List.length list) + prestej xs


(* c *)
let rec uporabi f = function 
    | Ponev x -> Ponev (f x)
    | Lonec (x, y) -> Lonec (f x, f y)
    | Omara l -> Omara (List.map f l)


(* d *)
let pospravi seznam = 
    let rec aux gradimo = function
        | [] -> Omara gradimo
        | x :: xs -> 
            match x with 
                | Ponev x ->  aux (x :: gradimo) xs
                | Lonec (x, y) ->  aux (x :: y :: gradimo) xs
                | Omara l ->  aux (l @ gradimo) xs
    in 
    aux [] seznam


(* e *)
let oceni seznam cenilka = 
    let rec aux grad = function 
        | [] -> grad 
        | x :: xs -> 
            match x with 
                | Ponev x -> aux (grad + (cenilka x)) xs
                | Lonec (x, y) -> aux (grad + 3 * ((cenilka x) + cenilka y)) xs
                | Omara l -> aux (grad + 5 * List.fold_left (+) 0 (List.map cenilka l)) xs
    in 
    aux 0 seznam