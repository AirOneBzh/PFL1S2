let rec min_list inf l = match l with [] -> failwith "liste vide" |[x] -> x |x::y::[] -> if inf x y then x else y |x::y::r -> if inf x y then min_list inf (x::r) else min_list inf (y::r);;

let rec supprime x l = match l with [] -> l |[n] -> if n==x then [] else [] |n::r -> if n==x then r else n::(supprime x r);;

let rec tri_selection_min inf l = match l with [] -> [] |[x] -> [x] |x::r -> (min_list inf l)::(tri_selection_min inf (supprime (min_list inf l) l));;

let rec partitionne l i j  = match l with [] -> (i, j) |x::[] -> (x::i, j) |x::y::r -> partitionne r (x::i) (y::j);;

let rec reverse l lr = match l with [] -> lr | x::r -> reverse r (x::lr);;

let rec fusionne inf l1 l2 l = match (l1,l2) with ([],_) -> (reverse l2 []) @ l | (_,[]) -> (reverse l1 []) @ l | (x1::r1,x2::r2) -> if inf x1 x2 then fusionne inf r1 l2 (x1::l) else fusionne inf l1 r2 (x2::l);;

let tri inf l = let (l1,l2) = partitionne l [] [] in reverse ( fusionne inf ( tri_selection_min inf l1 ) ( tri_selection_min inf l2) [] ) [];;

let rec suppr_doublons l = match l with [] -> l |[x] -> [x] |x::y::r -> if x == y then suppr_doublons (y::r) else x::( suppr_doublons (y::r));;
