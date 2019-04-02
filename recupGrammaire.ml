#use "type.ml";;

let rec contient x l =
    match l with
    | [] -> false
    | head::tail -> if head = x
                    then true
                    else contient x tail
    ;;

let  list_to_set liste =
    let rec list_to_set_rec liste ensemble =
        match liste with
        | [] -> ensemble
        | head::tail -> if List.mem head ensemble
                        then list_to_set_rec tail ensemble
                        else list_to_set_rec tail (head::ensemble)
    in list_to_set_rec liste []
    ;;

let rec recuperer_non_terminaux_regle production =
    let rec recuperer_non_terminaux_regle_rec production res =
        match production with
        | Prod(nt, liste) -> match liste with
                             | [] -> nt::res
                             | Epsilon::[] -> nt::res
                             | T(_)::tail -> recuperer_non_terminaux_regle_rec (Prod(nt, tail)) res
                             | NT(x)::tail -> recuperer_non_terminaux_regle_rec (Prod(nt, tail)) (NT(x)::res)

    in (list_to_set (recuperer_non_terminaux_regle_rec production []))
    ;;

let rec recuperer_non_terminaux_grammaire grammaire =
    match grammaire with
    | [] -> []
    | head::tail -> list_to_set ((recuperer_non_terminaux_regle head) @ (recuperer_non_terminaux_grammaire tail))
    ;;

let rec recuperer_regles nonterminal grammaire =
    match grammaire with
    | [] -> []
    | Prod(nt, l)::tail -> if nt = nonterminal
                           then Prod(nt, l)::(recuperer_regles nonterminal tail)
                           else recuperer_regles nonterminal tail
    ;;


let rec recuperer_non_terminaux_productibles non_terminal grammaire acc =
    if List.mem non_terminal acc
    then acc
    else let regles = recuperer_regles nonterminal grammaire in
             match regles with
             | [] -> acc
             | head::tail -> let nt = recuperer_non_terminaux_regle head in
                                 match nt with
                                 | [] ->





(*  Pour un non-terminal donne, recuperer toutes ses productions (regles dont il est member gauche)
        si il n'a pas de production, il n'est pas productible
        si il a des production :
            pour chaque production, verifier si elle est productible et l'ajouter a la liste des parcourus
            si une production n'est pas productible, ajouter la liste des productions parcourues et passer a la production suivante
            si une production est productible, ajouter ses non-terminaux et la liste des productions parcourues *)


(*  Pour chaque production, recuperer ses non-terminaux
        si il n'y en a pas, le membre de gauche de la production est productible
        sinon, pour chaque non-terminal, verifier si il n'a pas deja ete marque comme accessible
            si oui, passer au terminal suivant
            sinon, recuperer ses productions non encore parcourues *)
