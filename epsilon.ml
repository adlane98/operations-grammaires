#use "recupGrammaire.ml"

let rec retirer_terme_grammaire terme gram =
    match gram with
    | [] -> []
    | Prod(x, _)::tail when x = terme -> retirer_terme_grammaire terme tail
    | Prod(x, droite)::tail when (List.mem terme droite) -> if (List.length droite) = (nombre_occurences terme droite)
                                                            then retirer_terme_grammaire terme tail
                                                            else Prod(x, retirer_terme terme droite)::(retirer_terme_grammaire terme tail)
    | Prod(x, droite)::tail -> Prod(x, droite)::(retirer_terme_grammaire terme tail)
;;

let rec epsilon_seul nt gram =
    match gram with
    | [] -> true
    | Prod(x, [Epsilon])::tail when x = nt -> epsilon_seul nt tail
    | Prod(x, droite)::tail when (x = nt && List.length droite = nombre_occurences x droite) -> epsilon_seul nt tail
    | Prod(x, _)::tail when x = nt -> false
    | _::tail -> epsilon_seul nt tail
;;

let non_terminaux_produisent_epsilon gram =
    let rec non_terminaux_produisent_epsilon_rec gram gramCheck =
        match gram with
        | [] -> []
        | Prod(nt, [Epsilon])::tail -> (nt, (epsilon_seul nt gramCheck))::(non_terminaux_produisent_epsilon_rec tail gramCheck)
        | Prod(nt, _)::tail -> (non_terminaux_produisent_epsilon_rec tail gramCheck)
    in non_terminaux_produisent_epsilon_rec gram gram
;;


(* cas ou le membre de gauche associee a l'epsilon-regle apparait a gauche dans une autre regle *)
(* Test
dupliquer (NT('S')) (Prod(NT('S'), [T('a');NT('S');NT('T');T('b');NT('S');T('c');NT('S')]));;
*)
let dupliquer terme regle =
    match regle with
    | Prod(nt, droite) ->   let rec dupliquer_rec positions regle =
                                match positions with
                                | [] -> [Prod(nt, droite)]
                                | head::tail -> Prod(nt, (retirer_indices head droite))::(dupliquer_rec tail regle)
                            in dupliquer_rec (toutes_combinaisons (positions_valeurs terme droite)) regle
;;

(* cas ou le membre de gauche associee a l'epsilon-regle n'apparait aucune autre fois a gauche dans une autre regle *)
let rec retirer_lettre_grammaire terme gram =
    match gram with
    | [] -> []
    | Prod(nt, droite)::tail when (List.mem terme droite) -> (Prod(nt, retirer_terme terme droite))::(retirer_lettre_grammaire terme tail)
    | head::tail -> head::(retirer_lettre_grammaire terme tail)
;;

let rec retirer_lettre_production terme production =
    match production with
    | Prod(nt, droite) -> Prod(nt, retirer_terme terme droite)

let rec retirer_production_vide gram =
    match gram with
    | [] -> []
    | Prod(nt, [])::tail -> retirer_production_vide tail
    | head::tail -> head::(retirer_production_vide tail)
;;


let rec epsilon_iteration (terme, eps_seul) gram =
    match gram with
    | [] -> []
    | Prod(nt, [Epsilon])::tail when nt = terme -> epsilon_iteration (terme, eps_seul) tail
    | Prod(nt, droite)::tail when (List.mem terme droite) ->
        (if (eps_seul)
        then
            [retirer_lettre_production terme (Prod(nt, droite))]
        else dupliquer terme (Prod(nt, droite)))
        @
        (epsilon_iteration (terme, eps_seul) tail)
    | head::tail -> head::(epsilon_iteration (terme, eps_seul) tail)
;;

let supprimer_epsilon_regle gram =
    let rec supprimer_epsilon_regle_rec ntpe gram =
        match ntpe with
        | [] -> gram
        | head::tail -> supprimer_epsilon_regle_rec tail (epsilon_iteration head gram)
    in retirer_production_vide (supprimer_epsilon_regle_rec (non_terminaux_produisent_epsilon gram) gram)
;;
