#use "type.ml"


(*
 * Renvoie la liste 'liste' sans doublons
 *)
let  list_to_set liste =
    let rec list_to_set_rec liste ensemble =
        match liste with
        | [] -> ensemble
        | head::tail ->   if List.mem head ensemble then
                        list_to_set_rec tail ensemble
                    else
                        list_to_set_rec tail (head::ensemble)
    in list_to_set_rec liste []
    ;;


(*
 * Verifie si une liste est sous-liste d'une autre liste
 * @param deux listes
 * @return un booleen
 *)
let rec subset sub set =
    match sub with
    | [] -> true
    | h::t -> if (List.mem h set)
              then subset t set
              else false
    ;;


(*
 * Renvoie les non terminaux presents dans la regle 'production'
 * @param regle dont on souhaite determiner les non terminaux
 * @return une liste de non terminaux
 *)
let rec recuperer_non_terminaux_regle production =
    let rec recuperer_non_terminaux_regle_rec production res =
        match production with
        | Prod(nt, liste) ->    match liste with
                                | T(_)::tail | Epsilon::tail -> recuperer_non_terminaux_regle_rec (Prod(nt, tail)) res
                                | NT(x)::tail -> recuperer_non_terminaux_regle_rec (Prod(nt, tail)) (NT(x)::res)
                                | _ -> nt::res
    in (list_to_set (recuperer_non_terminaux_regle_rec production []))
    ;;


(*
 * Renvoie les non terminaux produits par la regle 'production'
 * @param regle dont on souhaite determiner les non terminaux produits
 * @return une liste de non terminaux
 *)
let rec non_terminaux_produits production =
    match production with
    | Prod(nt, h::t) -> begin
        match h with
        | NT(x) -> (NT(x))::(non_terminaux_produits (Prod(nt, t)))
        | _ -> non_terminaux_produits (Prod(nt, t))
        end
    | _ -> []
    ;;


(*
 * Renvoie les non terminaux d'une grammaire
 * @param grammaire une liste de regles
 * @return une liste de non terminaux
 *)
let rec recuperer_non_terminaux_grammaire grammaire =
    match grammaire with
    | [] -> []
    | head::tail -> list_to_set
                    ((recuperer_non_terminaux_regle head)
                    @
                    (recuperer_non_terminaux_grammaire tail))
    ;;


(*
 * Retire un symbole d'une liste de symboles donnee en parametre
 * @param alphabet une liste de symboles non terminaux et/ou terminaux
 * @return une liste de symboles non terminaux et/ou terminaux
 *)
let retirer_terme terme alphabet =
    let rec retirer_terme_rec terme alphabet res =
        match alphabet with
        | [] -> res
        | head::tail -> if head = terme
                        then retirer_terme_rec terme tail res
                        else retirer_terme_rec terme tail (head::res)
    in List.rev (retirer_terme_rec terme alphabet [])
    ;;


(*
 * Compte le nombre d'occurences d'un symbole 'terme' dans 'alphabet'
 *)
let rec nombre_occurences terme alphabet =
    match alphabet with
    | [] -> 0
    | head::tail -> if head = terme
                    then 1 + (nombre_occurences terme tail)
                    else nombre_occurences terme tail
    ;;


(*
 * Renvoie une liste contenant la position de chaque occurence de 'n' d
 * dans 'liste'.
 *)
let positions_valeurs n liste =
    let rec positions_valeurs_rec n liste i =
        match liste with
        | [] -> []
        | head::tail -> if head = n
                        then i::(positions_valeurs_rec n tail (i + 1))
                        else positions_valeurs_rec n tail (i + 1)
    in positions_valeurs_rec n liste 0
    ;;


(*
 * Retire l'element d'indice 'n' de la liste 'liste'.
 *)
let rec retirer_indice n liste =
    match liste with
    | [] -> []
    | head::tail -> if n = 0
                    then tail
                    else head::(retirer_indice (n - 1) tail)
    ;;


(*
 * Retire les elements d'indice contenu dans la liste 'n' de la liste 'liste'.
 *)
let rec retirer_indices ns liste =
    match ns with
    | [] -> liste
    | head::tail -> let moinsun x = x - 1 in
                    retirer_indices (List.map moinsun tail) (retirer_indice head liste)
    ;;


(*
 * Genere toutes les combinaisons de 'n' elements de la liste 'liste'.
 *)
let rec combinaisons n liste =
    match n with
    | 0 -> [[]]
    | _ ->  match liste with
            | [] -> []
            | head::tail -> let inserer_tete suite = head::suite in
                                (List.map inserer_tete (combinaisons (n - 1) tail))
                                @
                                (combinaisons n tail)
    ;;


(*
 * Genere toutes les combinaisons possibles de la liste 'liste'.
 *)
let toutes_combinaisons liste =
    let rec toutes_combinaisons_rec n liste =
        match n with
        | 0 -> []
        | _ -> (toutes_combinaisons_rec (n - 1) liste) @ (combinaisons n liste)
    in toutes_combinaisons_rec (List.length liste) liste
    ;;
