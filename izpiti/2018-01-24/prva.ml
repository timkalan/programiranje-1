(* a *)
let zapisi_vsa_stevila sez = 
    List.iter print_int sez


(* b *)
let map2_opt f sez1 sez2 = 
    let rec aux acc s1 s2 = 
        match (s1, s2) with 
            | ([], []) -> Some acc
            | (_, []) | ([], _) -> None
            | (x :: xs, y :: ys) -> aux ((f x y) :: acc) xs ys
    in 
    aux [] sez1 sez2

        
    