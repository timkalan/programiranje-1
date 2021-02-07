(* a *)
let razlika_produkta_in_vsote n1 n2 = 
    n1 * n2 - (n1 + n2)


(* b *)
let zlimaj_para (a, b) (c, d) = (a, b, c, d)


(* c *)
let trojica_graficno (x, y, z) = 
    let aux = function
        | None -> "-"
        | Some x -> string_of_int x
    in 
    "(" ^ aux x ^ ", " ^ aux y ^ ", " ^ aux z ^ ")"


(* d *)
let nedeljivo_do x n = 
    let rec aux grad = 
        if x mod grad = 0 then false 
        else if grad = n-1 then true
        else aux (grad+1) 
    in 
    aux 2


(* e *)
let razcepi_pri_None list = 
    let rec aux grad pod delam = 
        match pod with 
            | [] -> (delam |> List.rev) :: grad 
            | x :: xs -> 
                match x with 
                    | Some y -> aux grad xs (y :: delam)
                    | None -> aux ((delam |> List.rev) :: grad) xs []

    in 
    aux [] list [] |> List.rev

