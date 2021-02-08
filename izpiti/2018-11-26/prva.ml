(* 1 *)
let vsota sez = 
    let rec aux acc = function 
        | [] -> acc
        | x :: xs -> aux (x + acc) xs
    in 
    aux 0 sez

let vsota_fold list = 
    List.fold_left (+) 0 list


(* 2 *)
let rec urejen = function 
    | [] -> true 
    | x :: y :: xs -> 
        if y < x then false else
        urejen (y :: xs)
    | x :: [] -> true


(* 3 *)
let rec vstavi st = function 
    | [] -> [st]
    | x :: xs -> if x >= st then st :: x :: xs
        else x :: vstavi st xs


let uredi_vstavljanje seznam = 
    let rec aux acc = function 
        | [] -> acc
        | x :: xs -> aux (vstavi x acc) xs 
    in 
    aux [] seznam


(* 4 *)
let rec vstavi2 st cmp = function 
    | [] -> [st]
    | x :: xs -> if (cmp st x) then st :: x :: xs
        else x :: vstavi st xs


let urejanje sez cmp = 
    let rec aux acc = function 
        | [] -> acc
        | x :: xs -> aux (vstavi2 x cmp acc) xs 
    in 
    aux [] sez


(* 5 *)
type priority = Top | Group of int

type status = Staff | Passenger of priority

type flyer = { status : status ; name : string }

let flyers = [ 
    {status = Staff; name = "Quinn"}; 
    {status = Passenger (Group 0); name = "Xiao"}; 
    {status = Passenger Top; name = "Jaina"}; 
    {status = Passenger (Group 1000); name = "Aleks"}; 
    {status = Passenger (Group 1000); name = "Robin"}; 
    {status = Staff; name = "Alan"}
    ]


(* 6 *)
let vkrcaj sez = 
    let cmp p1 p2 = 
        match (p1.status, p2.status) with 
            | (Staff, _) -> true 
            | (Passenger Top, Passenger _) -> true
            | (Passenger (Group x), Passenger (Group y)) -> if x >= y then false else true
            | (Passenger (Group _), Passenger Top)
            | (Passenger _, Staff) -> false
    in 
    urejanje sez cmp


(* 7 *)
let razdeli sez = 
    let rec aux acc curr = function
        | [] -> curr :: acc
        | x :: xs -> 
            if (curr = []) || (x.status = (List.hd curr).status)
            then aux acc (x :: curr) xs
            else aux (curr :: acc) [] xs
    in 
    aux [] [] (vkrcaj sez) |> List.rev 


    