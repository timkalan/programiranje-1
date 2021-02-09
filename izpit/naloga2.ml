(*============================================================================*]
  Za učinkovitejše iskanje po leksikografsko urejenih parih bomo uporabili
  leksikografska drevesa, ki jih ustvarimo s pomočjo dvojiških dreves.

    type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

  Leksikografsko drevo za pare tipa ['a * 'b] je dvojiško drevo, ki ima v
  vozlišču element tipa ['a] (da lahko primerjamo po prvi komponenti) in pa
  drevo tipa ['b tree] (za primerjanje po drugi komponenti).

    type ('a, 'b) lexi_tree = ('a * 'b tree) tree

  Par [(a, b)] se nahaja v leksikografskem drevesu, če imamo v drevesu vozlišče
  s parom [(a, subtree)] in se [b] nahaja v [subtree]. 

  Primer drevesa za pare (3, "g"), (3, "t"), (7, "a"), (10, "e"), (10, "r"),
  (10, "t") in (10, "z") je:
          
          (7)--------┐
           |   "a"   |
           └---------┘
          /           \
         /             \
    (3)-------┐     (10)-----------┐
     | "g"    |      |     "r"     |
     |    \   |      |    /   \    |
     |    "t" |      |  "e"   "z"  |
     └--------┘      |       /     |
                     |     "t"     |
                     └-------------┘

[*============================================================================*)

type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

type ('a, 'b) lexi_tree = ('a * 'b tree) tree


(* a *)
(*============================================================================*]
  Definirajte primer, ki ustreza zgornjemu leksikografskemu drevesu.

[*============================================================================*)

(* funkcija iz vaj - lažji zapis listov*)
let leaf x = Node (Empty, x, Empty)

let primer : ((int, string) lexi_tree) = Node (
    leaf (3, Node (Empty, "g", leaf "t")), 
    (7, leaf "a"), 
    leaf (10, Node (leaf "e", "r", Node (leaf "t", "z", Empty)))
)


(* b *)
(*============================================================================*]
  Napišite funkcijo, ki preveri ali je par prisoten v leksikografskem drevesu.
[*============================================================================*)

(* funkcija iz vaj - member navadnega drevesa*)
let rec member x = function
    | Empty -> false
    | Node (l, y, r) -> 
        if (x < y) then member x l
        else if (x > y) then member x r
        else x = y

let rec preveri (a, b) (ltree : ('a, 'b) lexi_tree) =  
    match ltree with 
        | Empty -> false 
        | Node (l, (v, drevo), r) ->
            if a = v then member b drevo 
            else (preveri (a, b) l) || (preveri (a, b) r)

(* c *)
(*============================================================================*]
  Napišite funkcijo za vstavljanje elementov v leksikografsko drevo.
[*============================================================================*)

(* funkcija iz vaj - vstavljanje v navadno drevo *)
let rec insert x = function 
    | Empty -> leaf x 
    | Node (l, y, r) -> 
        if (x < y) then Node (insert x l, y, r)
        else if (x > y) then Node (l, y, insert x r)
        else Node (l, x, r)

let rec vstavi (a, b) (ltree : ('a, 'b) lexi_tree) : ('a, 'b) lexi_tree  = 
    match ltree with 
        | Empty -> leaf (a, insert b Empty)
        | Node (l, (v, drevo), r) ->
            if a < v then Node (vstavi (a, b) l, (v, drevo), r)
            else if a > v then Node (l, (v, drevo), vstavi (a, b) r)
            else Node (l, (v, insert b drevo), r)



(* d *)
(*============================================================================*]
  Napišite funkcijo [lexi_fold], ki sprejme funkcijo [f] in začetno vrednost
  akumulatorja, nato pa funkcijo zloži preko leksikografskega drevesa. Vrstni
  red zlaganja je določen z leksikografsko urejenostjo.

    lexi_fold : ('a -> 'b -> 'c -> 'a) -> 'a -> ('b, 'c) lexi_tree -> 'a
[*============================================================================*)

(* vir: https://stackoverflow.com/questions/4189514/fold-tree-in-ocaml *)
let rec fold_tree f a t = 
  match t with
    | Empty -> a 
    | Node (l, x, r) -> f x (fold_tree f a l) (fold_tree f a r)


let rec lexi_fold f acc ltree = 
    match ltree with 
        | Empty -> acc
        | Node (l, (v, drevo), r) -> 
            f (fold_tree (f acc v) drevo) (lexi_fold f acc l) (lexi_fold f acc r)


(* e *)
(*============================================================================*]
  Napišite funkcijo, ki vrne urejen seznam vseh elementov, ki se nahajajo v
  leksikografskem drevesu.
[*============================================================================*)
      
(* iz enega vozl. lexi_tree pridela seznam parov *)
let rec node_pairs v tr = 
    match tr with
        | Empty -> []
        | Node (l, x, r) -> (node_pairs v l) @ [(v, x)] @ (node_pairs v r)


let rec list_lexi = function
    | Empty -> []
    | Node (l, (v, tr), r) -> (list_lexi l) @ (node_pairs v tr) @ (list_lexi r)