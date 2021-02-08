type 'a tree = 
    | Prazno 
    | Vozlisce of 'a tree * 'a * 'a tree


let monotona_pot drevo = 
    let aux acc = function 
        | Prazno -> acc
        | 