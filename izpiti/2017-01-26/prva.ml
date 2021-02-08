type izraz = 
    | Samo of int 
    | Plus of izraz * izraz
    | Krat of izraz * izraz

let test = Krat (
    Plus (Samo 1, Samo 2), 
    Krat (Samo 3, Samo 4)
)

(* a *)
let rec izracunaj izraz = 
    match izraz with
        | Samo x -> x
        | Plus (x, y) -> izracunaj x + izracunaj y
        | Krat (x, y) -> izracunaj x * izracunaj y