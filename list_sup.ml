
let rec selectionne inf l = match l with [] -> failwith "liste vide" |[x] -> x |x::y::[] -> if inf x y then x else y |x::y::r -> if inf x y then selectionne inf (x::r) else selectionne inf (y::r);;
                
let rec supprime x l = match l with [] -> l |[n] -> if n==x then [] else [] |n::r -> if n==x then r else n::(supprime x r);;
