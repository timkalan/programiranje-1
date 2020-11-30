(* 1. PROBLEM *)
let rec last = function
    | [] -> None
    | [x] -> Some x
    | x :: xs -> last xs


(* 2. PROBLEM *)
let rec last_two = function
    | [] | [_] -> None
    | [x; y] -> Some (x, y)
    | _ :: xs -> last_two xs


(* 3. PROBLEM *)
let rec at k list = 
    match list with
    | [] -> None
    | x :: xs -> if k = 1 then x else at (k-1) list


(* 4. PROBLEM *)
let rec length = function
    | [] -> 0
    | x :: xs -> 1 + length xs

let length_tail sez = 
    let rec length_aux len list = 
        match list with
        | [] -> len
        | x :: xs -> length_aux (len+1) xs
    in 
    length_aux 0 sez


(* 5. PROBLEM *)
let rec rev = function
    | [] -> []
    | x :: xs -> (rev xs) @ [x] 

let rev_tail list =
    let rec rev_aux acc = function 
        | [] -> acc
        | x :: xs -> rev_aux (x :: acc) xs
    in 
    rev_aux [] list


(* 6. PROBLEM *)
let rec is_palindrome list = 
    list = rev_tail list

let is_palindrome_surovo list = 
    let rec pal_aux acc = function
        | [] -> acc = list
        | x :: xs -> pal_aux (x :: acc) xs
    in 
    pal_aux [] list


(* 7. PROBLEM *)
type 'a node =
    | One of 'a 
    | Many of 'a node list

let rec flatten = function
    | _ -> true