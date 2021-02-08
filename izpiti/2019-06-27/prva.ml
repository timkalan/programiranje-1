type complex = { re : float ; im : float }

(* a *)
let complex_add x y = {re = x.re +. y.re; im = x.im +. y.im}

(* b *)
let complex_conjugate x = {re = x.re; im = -. x.im}

(* c *)
let rec list_apply_either pred f g xs = 
    match xs with 
        | [] -> []
        | y :: ys -> 
            if pred y then (f y) :: (list_apply_either pred f g ys)
            else (g y) :: (list_apply_either pred f g ys)

(* d *)
let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)


let eval_poly koef t = 
    let rec aux acc potenca = function 
        | [] -> acc
        | x :: xs -> aux (x * (pow t potenca) + acc) (potenca + 1) xs
    in 
    aux 0 0 koef