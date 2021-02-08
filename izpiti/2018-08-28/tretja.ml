type 'a veriga = 
    | Filter of ('a -> bool) * 'a list * 'a veriga
    | Ostalo of 'a list


(* a *)
let test = 
    Filter (
        (fun x -> x < 0), 
        [], 
        Filter (
            (fun x -> x < 10), 
            [], 
            Ostalo []
        )
    )

(* b *)
let rec vstavi a veriga = 
    match veriga with 
        | Ostalo x -> Ostalo (a :: x)
        | Filter (f, drzi, v) ->
            if f a then Filter (f, a :: drzi, v)
            else Filter (f, drzi, vstavi a v)


(* c *)
let rec poisci a veriga = 
    match veriga with 
        | Ostalo x -> List.mem a x
        | Filter (f, drzi, v) ->  
            if f a then List.mem a drzi 
            else poisci a v


(* d *)
let izprazni_filtre veriga = 
    let rec aux acc = function 
        | Ostalo x -> acc @ x
        | Filter (f, drzi, v) -> 
            aux (acc @ drzi) v
    in 
    let rec aux2 veriga = 
        match veriga with 
            | Ostalo x -> Ostalo []
            | Filter (f, drzi, v) -> Filter (f, [], aux2 v)
    in
    (aux2 veriga, aux [] veriga)


(* e *)
let dodaj_filter f veriga = 
    let dodaj_prazno f veriga =
        match veriga with 
            | Ostalo x -> Filter (f, [], Ostalo [])
            | Filter (g, drzi, v) -> Filter (f, [], Filter (g, [], v))
    in 
    let prazna, elementi = izprazni_filtre veriga 
    in 
    let nova = dodaj_prazno f prazna 
    in 
    let rec aux acc = function 
        | [] -> acc 
        | x :: xs -> aux (vstavi x acc) xs
    in 
    aux nova elementi







    



