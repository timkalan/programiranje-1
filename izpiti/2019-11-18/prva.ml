(* a *)
let is_root x y = 
    (x * x = y) && (x >= 0) && (y >= 0)


(* b *)
let pack3 a b c = (a, b, c)


(* c *)
let sum_if_not p list = 
    let rec aux acc = function 
        | [] -> acc
        | x :: xs -> if (p x) then aux acc xs 
            else aux (x+acc) xs
    in 
    aux 0 list


(* d *)
let apply f_list list = 
    let rec aux acc f = function
        | [] -> acc
        | x :: xs -> aux ((f x) :: acc) f xs
    in 
    let rec aux2 acc = function 
        | [] -> acc
        | f :: fs -> aux2 ((aux [] f list) :: acc) fs
    in 
    aux2 [] f_list


