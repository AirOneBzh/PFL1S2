##Tri par sélection du minimum
let rec selectionne inf l = match l with [] -> failwith "liste vide" |[x] -> x |x::y::[] -> if inf x y then x else y |x::y::r -> if inf x y then selectionne inf (x::r) else selectionne inf (y::r);;
                
let rec supprime x l = match l with [] -> l |[n] -> if n==x then [] else [] |n::r -> if n==x then r else n::(supprime x r);;
        
let rec tri_selection_min inf l = match l with [] -> [] |[x] -> [x] |x::r -> (selectionne inf l)::(tri_selection_min inf (supprime (selectionne inf l) l));;
        
##Tri par partition-fusion
let rec partitionne_bis l i j  = match l with [] -> (i, j) |x::[] -> (x::i, j) |x::y::r -> partitionne_bis r (x::i) (y::j);;
   

let partitionne l = partitionne_bis l [] [];; 

# Fonction pour inverser une liste nous permet de gagner du temps en réutilisant partitionne qui séparait correctement la liste en deux liste inverses à celles voulues

let rec reverse_bis l lr = match l with [] -> lr | x::r -> reverse_bis r (x::lr);;

let reverse l= reverse_bis l [];;

let rec fusionne_bis inf l1 l2 l = match (l1,l2) with ([],_) -> (reverse l2) @ l | (_,[]) -> (reverse l1) @ l | (x1::r1,x2::r2) -> if inf x1 x2 then fusionne_bis inf r1 l2 (x1::l) else fusionne_bis inf l1 r2 (x2::l);;

let fusionne inf l1 l2 = reverse (fusionne_bis inf l1 l2 []);;

let tri_partition_fusion inf l = let (l1,l2) = partitionne l in fusionne inf ( tri_selection_min inf l1 ) ( tri_selection_min inf l2);;
