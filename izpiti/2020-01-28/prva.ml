(* a *)
let option_sum x y = 
    match (x, y) with
        | (None, _) | (_, None) -> None
        | (Some x, Some y) -> Some (x + y)


(* b *)
let twostep_map f g h arg = 
    let (x, y) = f arg in 
    let a = g x and b = h y in 
    (a, b)


(* c *)
(*let function_repeat f xs = 
    let rec aux grad = function 
        | [] -> grad 
        | y :: ys -> 
            *)


(* d *)
let iterate f pogoj vr = 
    let rec aux acc = 
        if pogoj acc then acc 
        else aux (f acc)
    in 
    aux vr
