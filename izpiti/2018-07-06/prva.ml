(* a *)
let uporabi f arg = f arg


(* b *)
let ibaropu arg f = f arg 


(* c *)
let zacetnih n xs = 
    if List.length xs < n then None
    else
    let rec aux acc st pod = 
        match pod with 
            | [] -> Some (List.rev acc) 
            | x :: xs when st > 0 -> aux (x :: acc) (st-1) xs
            | x :: xs when st <= 0 -> Some (List.rev acc) 
    in 
    aux [] n xs
        
