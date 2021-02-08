(* a *)
type vrsta_srecanja = Predavanja | Vaje

type srecanje = {predmet : string; vrsta : vrsta_srecanja; trajanje : int}

type urnik = (srecanje list) list


(* b *)
let vaje = {predmet = "Analiza 2a"; vrsta = Vaje; trajanje = 3}

let predavanja = {predmet = "Programiranje 1"; vrsta = Predavanja; trajanje = 2}


(* c *)
let urnik_profesor : urnik = [
    [{predmet = "Analiza 2a"; vrsta = Vaje; trajanje = 2}];
    [];
    [{predmet = "Programiranje 1"; vrsta = Predavanja; trajanje = 1}];
    [];
    [];
    [{predmet = "Analiza 2a"; vrsta = Vaje; trajanje = 1}];
    []
]


(* d *)
let rec je_preobremenjen (urnik : urnik) = 
    match urnik with 
        | [] -> false 
        | x :: xs -> 
            let rec aux acc = function
                | [] -> je_preobremenjen xs 
                | y :: ys -> 
                    if y.trajanje + acc > 4 then true 
                    else aux (acc + y.trajanje) ys
            in 
            aux 0 x 


(* e *)
let rec bogastvo (urnik : urnik) = 
    let rec aux acc = function
        | [] -> acc
        | x :: xs -> 
            if x.vrsta = Predavanja then aux (2 * x.trajanje) xs
            else aux (x.trajanje) xs
    in 
    match urnik with 
        | [] -> 0 
        | y :: ys -> (aux 0 y) + bogastvo ys
