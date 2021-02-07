type xytree =
    | Xsplit of int * xytree * xytree
    | Ysplit of int * xytree * xytree
    | Elements of (int * int) list


(* (a) *)
let example = Ysplit (
    2, 
    Xsplit (
        3, Elements [], Elements [(0, 2); (1, 1)]), 
    Xsplit (
        2, 
        Ysplit (
            4, Elements [(4, 3)], Elements []),
        Elements [(3, 1)]))


(* (b) *)
let num_of_elements drevo = 
    let rec aux acc = function
        | Elements x -> (List.length x) + acc
        | Xsplit (_, t1, t2) -> (aux acc t1) + (aux acc t2)
        | Ysplit (_, t1, t2) -> (aux acc t1) + (aux acc t2)
    in 
    aux 0 drevo


(* (c) *)
let rec insert (x, y) tree = 
    match tree with 
        | Elements a -> Elements ((x, y) :: a)
        | Xsplit (x', t1, t2) -> 
            if x' >= x then insert (x, y) t1
            else insert (x, y) t2
        | Ysplit (y', t1, t2) ->
            if y' >= y then insert (x, y) t1
            else insert (x, y) t2


(* d *)
let rec alternatives drevo = 
    match drevo with 
        | Elements _ -> true 
        | Xsplit (_, t1, t2) -> (
            match (t1, t2) with 
                | (Xsplit _, Xsplit _) -> false
                | (Xsplit _, _) -> false
                | (_, Xsplit _) -> false
                | (_, _) -> true && (alternatives t1) && (alternatives t2)
        )
        | Ysplit (_, t1, t2) -> (
            match (t1, t2) with 
                | (Ysplit _, Ysplit _) -> false
                | (Ysplit _, _) -> false
                | (_, Ysplit _) -> false
                | _ -> true && (alternatives t1) && (alternatives t2)
        )


(* e *)
