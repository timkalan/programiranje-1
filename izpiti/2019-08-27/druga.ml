type 'a gnezdenje = 
    | Element of 'a 
    | Podseznam of 'a gnezdenje list


let gnezdenje_primer = 
Podseznam [
    Element 1; Element 2; 
    Podseznam [
        Element 3; Podseznam [Element 4]; Podseznam []]; 
    Podseznam [
        Element 5]
    ]


let rec najvecja_globina_pom = function 
    | Element _ -> 0
    | Podseznam xs -> 1 + List.fold_left max 1 (List.map najvecja_globina_pom xs)
        
(* mal bedno tole al
let najvecja_globina g_list = 
    1 + List.fold_left max 1 (List.map najvecja_globina_pom g_list)
*)


let rec preslikaj f g = 
    match g with
    | Element x -> Element (f x)
    | Podseznam xs -> Podseznam ((List.map (preslikaj f)) xs)


let rec splosci = function
    | Element x -> [x]
    | Podseznam xs -> 
        let splosceni = List.map splosci xs in 
        List.fold_left (@) [] splosceni 

    
let rec alternirajoci_konstruktorji = function 
    | [] -> true 
    | [x] -> true 
    | Element _ :: Podseznam p::xs -> alternirajoci_konstruktorji ((Podseznam p)::xs)
    | Podseznam _ :: Element p::xs -> alternirajoci_konstruktorji ((Element p)::xs)
    | _ -> false



let rec zlozi_gnezdenje f acc g = 
    match g with 
    | Element x -> f acc x
    | Podseznam l -> 
        List.fold_left (zlozi_gnezdenje f) acc l


let zlozi_preko_gnezdenja f acc g_list =
    zlozi_gnezdenje f acc (Podseznam g_list)