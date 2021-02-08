(* a *)
type 'a improved_list = 
    | Empty
    | Sestavljen of 'a array * 'a improved_list

let test = Sestavljen (
    [|1; 2; 20|], 
    Sestavljen (
        [|17; 19; 20; 30|], 
        Sestavljen (
            [|100|], 
            Empty
        ))
)

(* b *)
let rec count = function 
    | Empty -> 0 
    | Sestavljen (x, xs) -> Array.length x + count xs


(* c *)
let nth list i = 
    let rec aux pogledali = function 
        | Empty -> None 
        | Sestavljen (x, xs) -> 
            if pogledali = i - 1 then Some 