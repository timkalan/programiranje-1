(* a *)
let angle_between (x1, x2) (y1, y2) = 
    let skalarc = x1 *. y1 +. x2 *. y2 in 
    let mag1 = sqrt (x1 *. x1 +. x2 *. x2) in 
    let mag2 = sqrt (y1 *. y1 +. y2 *. y2) in 
    acos (skalarc /. (mag1 *. mag2))


(* b *)
let list_to_triple list = 
    match list with 
        | [x; y; z] -> Some (x, y, z)
        | _ -> None


(* c *)
type counter = {
    lt : int; 
    eq : int;
    gt : int
}

let compare_with sez vr = 
    let rec aux lt eq gt = function
        | [] -> {lt = lt; eq = eq; gt = gt} 
        | x :: xs ->  
            if x > vr then aux lt eq (gt+1) xs
            else if x = vr then aux lt (eq+1) gt xs
            else aux (lt+1) eq gt xs
    in 
    aux 0 0 0 sez


(* d *)
let apply_all sez x = 
    let rec aux grad = function 
        | [] -> grad
        | f :: fs -> aux (f grad) fs
    in 
    aux x sez

