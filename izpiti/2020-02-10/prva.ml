(* a *)
let dot_prod (x1, y1, z1) (x2, y2, z2) =
    x1 *. x2 +. y1 *. y2 +. z1 *. z2


(* b *)
let fix_second f arg2 = fun arg -> f arg arg2


(* c *)
let combine_and_filter f xs ys = 
    let rec aux grad s1 s2 =  
        match (s1, s2) with 
            | (x :: xs, y :: ys) -> (
                match f x y with 
                    | Some z -> aux (z :: grad) xs ys
                    | None -> aux grad xs ys
            )
            | _ -> List.rev grad
    in 
    aux [] xs ys


(* d *)
let rec conditional_print p sez = 
    match sez with 
        | [] -> ()
        | x :: [] -> if p x then (print_string x)
            else conditional_print p []
        | x :: xs when p x -> print_string (x ^ ", ");
            conditional_print p xs;
        | x :: xs -> conditional_print p xs
    
