let rec selectionne inf l = match l with [] -> failwith "liste vide" |[x] -> x |x::y::[] -> if inf x y then x else |x::y::r -> if inf x y then selectionne inf (x::r) else selectionne inf (y::r);;
