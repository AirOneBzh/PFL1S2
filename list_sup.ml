##Tri par sélection du minimum
let rec selectionne inf l = match l with [] -> failwith "liste vide" |[x] -> x |x::y::[] -> if inf x y then x else y |x::y::r -> if inf x y then selectionne inf (x::r) else selectionne inf (y::r);;
                
let rec supprime x l = match l with [] -> l |[n] -> if n==x then [] else [] |n::r -> if n==x then r else n::(supprime x r);;
        
let rec tri_selection_min inf l = match l with [] -> [] |[x] -> [x] |x::r -> (selectionne inf l)::(tri_selection_min inf (supprime (selectionne inf l) l));;
        
##Tri par partition-fusion
let rec partitionne_bis l i j  = match l with [] -> (i, j) |x::[] -> (x::i, j) |x::y::r -> partitionne_bis r (x::i) (y::j);;
   

let partitionne l = partitionne_bis l [] [];; 
