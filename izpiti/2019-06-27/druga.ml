type lastnik = string

type vrt = 
    | Obdelovan of lastnik
    | Oddan of lastnik * (vrt * vrt list)
    | Prost

(* a *)
let vrt_primer = Oddan (
    "Kovalevskaya", 
    [Obdelovan ("Galois"), Obdelovan ("Lagrange"), Prost]
)