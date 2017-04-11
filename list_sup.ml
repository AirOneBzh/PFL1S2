
let selectionne inf l = match l with 
    [] -> failwith "liste vide" 
    |[x] -> x 
    |x::y::[] -> if inf x y then x else y
    |x::y::r -> if inf x y then 
