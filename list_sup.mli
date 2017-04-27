(* list_sup.mli *)

      (************************************)
      (*                                  *)
      (* Specification du module list_sup *)
      (*                                  *)
      (************************************)

(*************************************************)

val min_list : ('a -> 'a -> bool) -> 'a list -> 'a

(* ( min_list inf l ) permet de trouver le minimum
    d'une liste l suivant l'ordre établi par inf *)

(*************************************************)

val supprime : 'a -> 'a list -> 'a list

(* (supprime x l ) supprime le premier élément
            x de la liste l                 *)

(****************************************************************)

val tri_selection_min : ('a -> 'a -> bool) -> 'a list -> 'a list

(* (tri_selection_min inf l ) retourne une liste triée
       avec les même éléments que la liste l *)

(****************************************************************)

val partitionne : 'a list -> 'a list -> 'a list -> 'a list * 'a list

(* ( partitionne l i j ) sépare en deux listes la liste l,
        i et j sont les variables qui porteront les listes séparés
        durant les différents appels récursifs               *)

(****************************************************************)

val reverse : 'a list -> 'a list -> 'a list

(* ( reverse l lr )  retourne la liste inversée de l,
      durant les appels récursifs les premières valeurs de l
      s’ajouteront au début de lr ce qui permet de retourner la liste  *)

(****************************************************************)

val fusionne : ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list -> 'a list

(* ( fusionne inf l1 l2 l ) compare le premier élément de chaque
              liste l1 l2 puisqu'elles sont triées afin d'ajouter
              celui qui ressort de la comparaison inf dans la liste l   *)

(****************************************************************)

val tri : ('a -> 'a -> bool) -> 'a list -> 'a list

(* ( tri inf l ) permet d'appeler tour à tour les fonctions
        précédentes dans le but de trier rapidement une liste
        par la méthode de partition fusion               *)

(****************************************************************)

val suppr_doublons : 'a list -> 'a list

(* ( suppr_doublons l )  retourne la liste triée l sans
                      les éventuels doublons *)
