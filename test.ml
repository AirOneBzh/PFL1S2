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
