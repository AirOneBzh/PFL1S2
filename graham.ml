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

let rec tri_points_ter l ln =
match l with
[] -> ln
| n::[] -> ln@[n]
| n::s::r -> let b=min_list infc l in let y = tri_points_bis (supprime b l) b s in tri_points_ter (supprime y l) (ln@[y]);;

let tri_points l = let b=min_list infc l in let l2 = tri_points_ter (tri infc l) [] in b::suppr_doublons(l2);;

let rec algo_graham l pile =
match l with
[] -> pile
| x::r -> let l = tri_points l in if pile = vide
          then algo_graham r (empiler x pile)
          else if depiler pile = vide
               then algo_graham r (empiler x pile)
               else  if det (top pile) (subtop pile) x < 0
                     then algo_graham r (depiler  pile)
                     else if det (top pile) (subtop pile) x > 0
                          then algo_graham r (empiler x pile)
                          else algo_graham r (empiler x (depiler pile));;
