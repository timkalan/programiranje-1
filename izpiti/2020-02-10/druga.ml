type ('a, 'b) tree = 
    | Empty 
    | Anode of ('a, 'b) tree * 'a * ('a, 'b) tree
    | Bnode of ('a, 'b) tree * 'b * ('a, 'b) tree


(* a *)
let test = Anode (
    Bnode (Empty, true, Empty), 
    12, 
    Anode (
        Anode (Empty, 0, Empty), 
        5, 
        Bnode (Empty, false, Empty)
    )
)

let test2 = Anode (
    Bnode (Empty, true, Empty), 
    12, 
    Anode (
        Bnode (Empty, false, Empty), 
        5, 
        Bnode (Empty, false, Empty)
    )
)


(* b *)
let rec adepth = function 
    | Empty -> 0 
    | Anode (t1, x, t2) -> 1 + max (adepth t1) (adepth t2)
    | Bnode (t1, y, t2) -> max (adepth t1) (adepth t2)


let rec bdepth = function 
    | Empty -> 0 
    | Anode (t1, x, t2) -> max (bdepth t1) (bdepth t2)
    | Bnode (t1, y, t2) -> 1 + max (bdepth t1) (bdepth t2)


(* c *)
type result = {aNodes : int; bNodes : int}

let rec count drevo = 
    let rec auxA = function 
        | Empty -> 0
        | Anode (t1, _, t2) -> 1 + auxA t1 + auxA t2
        | Bnode (t1, _, t2) -> auxA t1 + auxA t2
    in 
    let rec auxB = function 
        | Empty -> 0
        | Anode (t1, _, t2) -> auxB t1 + auxB t2
        | Bnode (t1, _, t2) -> 1 + auxB t1 + auxB t2
    in 
    {aNodes = auxA drevo; bNodes = auxB drevo}


(* d *)
let foldmap fa fb acc tr = ()