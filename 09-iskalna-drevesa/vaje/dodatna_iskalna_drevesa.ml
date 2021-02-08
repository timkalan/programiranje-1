(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 DODATNE VAJE 
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(* ========== Vaja 4: Iskalna Drevesa  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Ocaml omogoča enostavno delo z drevesi. Konstruiramo nov tip dreves, ki so
 bodisi prazna, bodisi pa vsebujejo podatek in imajo dve (morda prazni)
 poddrevesi. Na tej točki ne predpostavljamo ničesar drugega o obliki dreves.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type 'a tree = Prazno | Vozlisce of 'a tree * 'a * 'a tree

(*----------------------------------------------------------------------------*]
 Definirajmo si testni primer za preizkušanje funkcij v nadaljevanju. Testni
 primer predstavlja spodaj narisano drevo, pomagamo pa si s pomožno funkcijo
 [leaf], ki iz podatka zgradi list.
          5
         / \
        2   7
       /   / \
      0   6   11
[*----------------------------------------------------------------------------*)

let leaf x = Vozlisce (Prazno, x , Prazno)

let testno_drevo = Vozlisce(
                   Vozlisce(leaf 0, 2, Prazno),
                   5, 
                   Vozlisce(leaf 6, 7, leaf 11))

(*----------------------------------------------------------------------------*]
 Funkcija [mirror] vrne prezrcaljeno drevo. Na primeru [test_tree] torej vrne
          5
         / \
        7   2
       / \   \
      11  6   0
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mirror test_tree ;;
 - : int tree =
 Node (Node (Node (Empty, 11, Empty), 7, Node (Empty, 6, Empty)), 5,
 Node (Empty, 2, Node (Empty, 0, Empty)))
[*----------------------------------------------------------------------------*)

(* iskalno drevo obrne v iskalno drevo za obratno relacijo *)
let rec mirror = function
    | Prazno -> Prazno
    | Vozlisce (levi, vr, desni) -> Vozlisce (mirror desni, vr, mirror levi)

(* da se repno rekurzivno s CPS *)

(*----------------------------------------------------------------------------*]
 Funkcija [height] vrne višino oz. globino drevesa, funkcija [size] pa število
 vseh vozlišč drevesa.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # height test_tree;;
 - : int = 3
 # size test_tree;;
 - : int = 6
[*----------------------------------------------------------------------------*)

(* obe sta O(n) -> pogledamo vse *)
(* Prostorska zahtevnost: O(height) *)
let rec height tree = 
    match tree with
    | Prazno -> 0 
    | Vozlisce (l, v, d) -> 1 + max (height l) (height d)


let rec size tree =
    match tree with
    | Prazno -> 0 
    | Vozlisce (l, v, d) -> 1 + size l + size d

(*----------------------------------------------------------------------------*]
 Funkcija [map_tree f tree] preslika drevo v novo drevo, ki vsebuje podatke
 drevesa [tree] preslikane s funkcijo [f].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # map_tree ((<)3) test_tree;;
 - : bool tree =
 Node (Node (Node (Empty, false, Empty), false, Empty), true,
 Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty)))
[*----------------------------------------------------------------------------*)

(* O(f n) *)
let rec map_tree f tree = 
    match tree with
    | Prazno -> Prazno
    | Vozlisce (l, v, d) -> Vozlisce (map_tree f l, f v, map_tree f d)

(*----------------------------------------------------------------------------*]
 Funkcija [list_of_tree] pretvori drevo v seznam. Vrstni red podatkov v seznamu
 naj bo takšen, da v primeru binarnega iskalnega drevesa vrne urejen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # list_of_tree test_tree;;
 - : int list = [0; 2; 5; 6; 7; 11]
[*----------------------------------------------------------------------------*)

(* in order traversal *)
(* O(n) - O(n**2) *)
let rec list_of_tree = function
    | Prazno -> []
    | Vozlisce (l, v, d) -> list_of_tree l @ (v :: list_of_tree d)

(*----------------------------------------------------------------------------*]
 Funkcija [is_bst] preveri ali je drevo binarno iskalno drevo (Binary Search 
 Tree, na kratko BST). Predpostavite, da v drevesu ni ponovitev elementov, 
 torej drevo npr. ni oblike Node( leaf 1, 1, leaf 2)). Prazno drevo je BST.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_bst test_tree;;
 - : bool = true
 # test_tree |> mirror |> is_bst;;
 - : bool = false
[*----------------------------------------------------------------------------*)

(*
let is_bst tree = 
    list_of_tree tree = List.sort (compare) (list_of_tree tree)
*)

let is_bst tree = 
    let l = list_of_tree tree in 
    let rec is_sorted = function 
        | [] -> true 
        | [_] -> true 
        | x :: y :: rest -> (x <= y) && (is_sorted (y :: rest))
    in 
    is_sorted l


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 V nadaljevanju predpostavljamo, da imajo dvojiška drevesa strukturo BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Funkcija [insert] v iskalno drevo pravilno vstavi dani element. Funkcija 
 [member] preveri ali je dani element v iskalnem drevesu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 2 (leaf 4);;
 - : int tree = Node (Node (Empty, 2, Empty), 4, Empty)
 # member 3 test_tree;;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec insert x = function 
    | Prazno -> leaf x 
    | Vozlisce (l, y, r) -> 
        if (x < y) then Vozlisce (insert x l, y, r)
        else if (x > y) then Vozlisce (l, y, insert x r)
        else Vozlisce (l, x, r)


let rec member x = function
    | Prazno -> false
    | Vozlisce (l, y, r) -> 
        if (x < y) then member x l
        else if (x > y) then member x r
        else true

(*----------------------------------------------------------------------------*]
 Funkcija [member2] ne privzame, da je drevo bst.
 
 Opomba: Premislte kolikšna je časovna zahtevnost funkcije [member] in kolikšna
 funkcije [member2] na drevesu z n vozlišči, ki ima globino log(n). 
[*----------------------------------------------------------------------------*)

let rec member2 x = function 
    | Prazno -> false 
    | Vozlisce (l, y, r) -> member2 x l || member2 x r || x = y

(*----------------------------------------------------------------------------*]
 Funkcija [succ] vrne naslednjika korena danega drevesa, če obstaja. Za drevo
 oblike [bst = Node(l, x, r)] vrne najmanjši element drevesa [bst], ki je večji
 od korena [x].
 Funkcija [pred] simetrično vrne največji element drevesa, ki je manjši od
 korena, če obstaja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # succ test_tree;;
 - : int option = Some 6
 # pred (Node(Empty, 5, leaf 7));;
 - : int option = None
[*----------------------------------------------------------------------------*)

(*
let succ = function
    | Prazno -> None
    | Vozlisce (l, x, r) -> Some (List.nth (list_of_tree r) 0)
*)

let rec minimal = function
    | Prazno -> None 
    | Vozlisce (Prazno, x, d) -> Some x 
    | Vozlisce (l, _, _) -> minimal l


let rec maximal = function
    | Prazno -> None 
    | Vozlisce (_, x, Prazno) -> Some x 
    | Vozlisce (_, _, d) -> minimal d


(* O(visina), repno rekurzivno *)
let succ = function
    | Prazno -> None
    | Vozlisce (_, _, r) -> minimal r


let pred = function
    | Prazno -> None
    | Vozlisce (l, _, _) -> maximal l

(*----------------------------------------------------------------------------*]
 Na predavanjih ste omenili dva načina brisanja elementov iz drevesa. Prvi 
 uporablja [succ], drugi pa [pred]. Funkcija [delete x bst] iz drevesa [bst] 
 izbriše element [x], če ta v drevesu obstaja. Za vajo lahko implementirate
 oba načina brisanja elementov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # (*<< Za [delete] definiran s funkcijo [succ]. >>*)
 # delete 7 test_tree;;
 - : int tree =
 Node (Node (Node (Empty, 0, Empty), 2, Empty), 5,
 Node (Node (Empty, 6, Empty), 11, Empty))
[*----------------------------------------------------------------------------*)

let rec delete x = function
    | Prazno -> Prazno
    | Vozlisce (l, y, r) -> 
        if x < y then 
            Vozlisce (delete x l, y, r)
        else if x > y then 
            Vozlisce (l, y, delete x r)

        else 
            let nas = succ (Vozlisce (l, y, r)) in
            match nas with
            | None -> l
            | Some nas -> Vozlisce (l, nas, delete nas r)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 SLOVARJI

 S pomočjo BST lahko (zadovoljivo) učinkovito definiramo slovarje. V praksi se
 slovarje definira s pomočjo hash tabel, ki so še učinkovitejše. V nadaljevanju
 pa predpostavimo, da so naši slovarji [dict] binarna iskalna drevesa, ki v
 vsakem vozlišču hranijo tako ključ kot tudi pripadajočo vrednost, in imajo BST
 strukturo glede na ključe. Ker slovar potrebuje parameter za tip ključa in tip
 vrednosti, ga parametriziramo kot [('key, 'value) dict].
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type ('key, 'value) dict = ('key * 'value) tree

(*----------------------------------------------------------------------------*]
 Napišite testni primer [test_dict]:
      "b":1
      /    \
  "a":0  "d":2
         /
     "c":-2
[*----------------------------------------------------------------------------*)

let test_dict = 
    Vozlisce (leaf ("a", 0), ("b", 1), Vozlisce (leaf ("c", -1), ("d", 2), Prazno))

(*----------------------------------------------------------------------------*]
 Funkcija [dict_get key dict] v slovarju poišče vrednost z ključem [key]. Ker
 slovar vrednosti morda ne vsebuje, vrne [option] tip.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_get "banana" test_dict;;
 - : 'a option = None
 # dict_get "c" test_dict;;
 - : int option = Some (-2)
[*----------------------------------------------------------------------------*)

let rec dict_get k = function 
    | Prazno -> None
    | Vozlisce (l, (k', v'), r) ->
        if k < k' then 
            dict_get k l 
        else if k > k' then 
            dict_get k r 
        else 
            Some v'
      
(*----------------------------------------------------------------------------*]
 Funkcija [print_dict] sprejme slovar s ključi tipa [string] in vrednostmi tipa
 [int] in v pravilnem vrstnem redu izpiše vrstice "ključ : vrednost" za vsa
 vozlišča slovarja.
 Namig: Uporabite funkciji [print_string] in [print_int]. Nize združujemo z
 operatorjem [^]. V tipu funkcije si oglejte, kako uporaba teh funkcij določi
 parametra za tip ključev in vrednosti v primerjavi s tipom [dict_get].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # print_dict test_dict;;
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let rec print_string_int_dict = function
    | Prazno -> ()
    | Vozlisce(l,(k,v),r) -> 
        let () = print_endline (k ^ " : " ^(string_of_int v)) in
        print_string_int_dict l;
        print_string_int_dict r

(*----------------------------------------------------------------------------*]
 Funkcija [dict_insert key value dict] v slovar [dict] pod ključ [key] vstavi
 vrednost [value]. Če za nek ključ vrednost že obstaja, jo zamenja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_insert "1" 14 test_dict |> print_dict;;
 1 : 14
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
 # dict_insert "c" 14 test_dict |> print_dict;;
 a : 0
 b : 1
 c : 14
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let rec dict_insert k v = function
    | Prazno -> leaf (k, v)
    | Vozlisce (l, (k', v'), r) ->
        if k < k' then 
            Vozlisce (dict_insert k v l, (k', v'), r)
        else if k > k' then     
            Vozlisce (l, (k', v'), dict_insert k v r)
        else
            Vozlisce (l, (k', v), r)

(*----------------------------------------------------------------------------*]
 Funkcija [bst_of_list] iz seznama naredi dvojiško iskalno drevo.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # [11; 6; 7; 0; 2; 5] |> bst_of_list |> is_bst;;
 - : bool = true
[*----------------------------------------------------------------------------*)

let bst_of_list list = 
    let rec aux acc = function
        | [] -> acc
        | x :: xs -> aux (insert x acc) xs
    in aux Prazno list

(*----------------------------------------------------------------------------*]
 Funkcija [tree_sort] uredi seznam s pomočjo pretvorbe v bst in nato nazaj
 v seznam.

 Opomba: Prosim ne uporabljajte te funkcije v praksi.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # tree_sort ["a"; "c"; "f"; "b"; "e"; "d"];;
 - : string list = ["a"; "b"; "c"; "d"; "e"; "f"]
[*----------------------------------------------------------------------------*)

let tree_sort sez = 
    bst_of_list sez |> list_of_tree

(*----------------------------------------------------------------------------*]
 Funkcija [follow directions tree] tipa [direction list -> 'a tree -> 'a option]
 sprejme seznam navodil za premikanje po drevesu in vrne vozlišče do katerega 
 vodi podana pot. Ker navodila morda ne vodijo do nobenega vozlišča v drevesu
 vrne rezultat kot [option] tip. Ne pozabite definirati tipa [directions].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # follow [Right; Left] test_tree;;
 - : int option = Some 6
 # follow [Right; Left; Right; Right] test_tree;;
 - : int option = None
[*----------------------------------------------------------------------------*)

type directions = Right | Left

let follow directions tree = 
    let aux dir drevo = 
        match (drevo, dir) with
            | (Prazno, _) -> Prazno
            | (Vozlisce (l, x, r), Right) -> r
            | (Vozlisce (l, x, r), Left) -> l
    in 
    let rec aux2 t = function 
        | [] -> t
        | x :: xs -> aux2 (aux x t) xs
    in 
    let dr = (aux2 tree directions) in 
        match dr with 
            | Prazno -> None
            | Vozlisce (_, x, _) -> Some x

(*----------------------------------------------------------------------------*]
 Funkcija [prune directions tree] poišče vozlišče v drevesu glede na navodila,
 ter izbriše poddrevo, ki se začne v izbranem vozlišču.

 Opozorilo: Pri uporabi [Some Node(l, x, r)] se OCaml pritoži, saj to razume 
 kot [(Some Node)(l, x, r)], zato pravilno postavite potrebne oklepaje.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # prune [Right] test_tree;;
 - : int tree option =
 Some (Node (Node (Node (Empty, 0, Empty), 2, Empty), 5, Empty))
[*----------------------------------------------------------------------------*)

let prune directions tree = 
    let aux dir drevo = 
        match (drevo, dir) with
            | (Prazno, _) -> Prazno
            | (Vozlisce (l, x, r), Right) -> r
            | (Vozlisce (l, x, r), Left) -> l
    in 
    let rec aux2 t = function 
        | [] -> t
        | x :: xs -> aux2 (aux x t) xs
    in 
    let bris = aux2 tree directions in 
    let rec aux3 tree = function
        | Prazno -> tree 
        | Vozlisce (l, x, r) -> failwith "aaaa"
    in 
    ()
    


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 PHANTOM TREES

 Druga možnost pri brisanju podatkov je, da spremenimo tip s katerim
 predstavljamo drevo. Definirate nov tip fantomskega drevesa, ki poleg podatka,
 levega in desnega poddrevesa hrani še dodatno informacijo o stanju [state], ki
 je bodisi [Exists] če je vozlišče še prisotno in pa [Ghost] če je vozlišče v
 drevesu izbrisano in ga upoštevamo le še kot delitveno vozlišče. Še vedno
 predpostavljamo, da imajo drevesa obliko BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type stanje = Exists | Ghost

type 'a phantom_tree = 
    | Prazno 
    | Vozlisce of 'a phantom_tree * 'a * 'a phantom_tree * stanje

(*----------------------------------------------------------------------------*]
 Funkcija [phantomize] tipa ['a tree -> 'a phantom_tree] navadnemu drevesu
 priredi ekvivalentno fantomsko drevo.
 Funkcija [kill x ptree] izbriše element [x] v fantomskem drevesu tako, da 
 njegovo stanje nastavi na [Ghost].
 Predpostavite lahko, da v drevesu ni ponovitev elementov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # phantomize test_tree;;
 - : int phantom_tree =
 P_Node (P_Node (P_Node (P_Empty, 0, P_Empty, Exists), 2, P_Empty, Exists), 5,
 P_Node (P_Node (P_Empty, 6, P_Empty, Exists), 7,
 P_Node (P_Empty, 11, P_Empty, Exists), Exists),
 Exists)

 # bst_of_list [3; 4; 2] |> phantomize |> kill 3 |> kill 6;;
 - : int phantom_tree =
 P_Node (P_Empty, 2,
 P_Node (P_Node (P_Empty, 3, P_Empty, Ghost), 4, P_Empty, Exists), Exists)
[*----------------------------------------------------------------------------*)

let rec phantomize (tree : 'a tree) = 
    match tree with
        | Prazno -> Prazno 
        | Vozlisce (l, x, r) -> Vozlisce (phantomize l, x, phantomize r, Exists)


let rec kill x ptree = 
    match ptree with 
        | Prazno -> Prazno
        | Vozlisce (l, y, r, state) -> 
            if y < x then Vozlisce (kill x l, y, r, state)
            else if y = x then Vozlisce (l, y, r, Ghost)
            else Vozlisce (l, y, kill x r, state)


(*----------------------------------------------------------------------------*]
 Funkcija [unphantomize] tipa ['a phantom_tree -> 'a tree] fantomskemu drevesu 
 priredi navadno drevo, ki vsebuje zgolj vozlišča, ki še obstajajo. Vrstni red
 vozlišč v končnem drevesu ni pomemben.

 Namig: Lahko uporabite vmesni prehodom na drugo podatkovno strukturo.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # testno_drevo |> phantomize |> kill 7 |> kill 0 |> kill 5 |> unphantomize;;
 - : int tree = Node (Node (Node (Empty, 2, Empty), 6, Empty), 11, Empty)
[*----------------------------------------------------------------------------*)

let unphantomize ptree : 'a tree = 
    let rec aux = function 
        | Prazno -> [] 
        | Vozlisce (l, x, r, Exists) -> aux l @ (x :: aux r)
        | Vozlisce (l, x, r, Ghost) -> aux l @ aux r
    in 
    let sez = aux ptree in 
    bst_of_list sez

