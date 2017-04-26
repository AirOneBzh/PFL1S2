open Point;;
open List_sup;;

let infc p1 p2 =  if p1.y < p2.y
                  then true
                  else  p1.y = p2.y && p1.x <= p2.x ;;

let det q0 q1 q2 = (q1.x - q0.x)*(q2.y - q0.y) - (q2.x - q0.x) * (q1.y - q0.y);;

let sca q0 q1 q2 = (q1.x - q0.x)*(q2.x - q0.x) + (q1.y - q0.y)*(q2.y - q0.y);;

let infg w p1 p2 =  p1 = w || p1 = p2 || (p1 <> w && p2 <> w && p1 <> p2 && (det w p1 p2) > 0) || (p1 <> w && p2 <> w && p1 <> p2 && (det w p1 p2) == 0 && (sca p1 w p2) < 0) ;;



let rec tri_points_bis l x y =
match l with
[] -> y
|z::r -> if infg x y z then tri_points_bis r x y else tri_points_bis r x z;;

let rec tri_points_ter l ln = let x=min_list infc l in let y = tri_points_bis (supprime x l) x {x=x.x+2;y=x.y-1} in
match l with
[]-> failwith "erreur"
| [x]-> ln
| n::r -> tri_points_ter (supprime y l) (ln@[y]);;

let tri_points l = suppr_doublons([min_list infc l]@(tri_points_ter l []));;
(*)
  let rec algo_graham l p =
  let b = top p in
  let a = subtop p in
  match l with
  [] -> p
  | x::r -> if det x a b <= 0 then algo_graham r (empiler x p) else algo_graham r p ;;
*)
#use "f.ml";;
