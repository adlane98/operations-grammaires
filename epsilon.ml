#use "productifs.ml";;

(* Fichier de la premiere methode *)

(*
 * Renvoie true si la grammaire 'gram' contient uniquement des
 * regles de la forme A -> epsilon et/ou A -> A..A (uniquement des A à droite)
 *)
let rec epsilon_seul nt gram =
    match gram with
    | [] -> true
    | Prod(x, [Epsilon])::tail when x = nt -> epsilon_seul nt tail
    | Prod(x, droite)::tail when (x = nt && List.length droite = nombre_occurences x droite) -> epsilon_seul nt tail
    | Prod(x, _)::tail when x = nt -> false
    | _::tail -> epsilon_seul nt tail
;;


(*
 * Renvoie une liste des symboles non terminaux qui produisent
 * epsilon dans la grammaire 'gram'.
 * La liste renvoyee contient des elements de la forme '(nt, cas)'
 * avec 'nt' le symbole non terminal et cas la valeur renvoye par 'epsilon_seul'
 * avec comme parametre 'nt'.
 *)
let non_terminaux_produisent_epsilon gram =
    let rec non_terminaux_produisent_epsilon_rec gram gramCheck =
        match gram with
        | [] -> []
        | Prod(nt, [Epsilon])::tail -> (nt, (epsilon_seul nt gramCheck))::(non_terminaux_produisent_epsilon_rec tail gramCheck)
        | Prod(nt, _)::tail -> (non_terminaux_produisent_epsilon_rec tail gramCheck)
    in non_terminaux_produisent_epsilon_rec gram gram
;;


(*
 * Cas 1.
 * Retire un symbole 'terme' de toute la grammaire 'gram'.
 *)
let rec retirer_lettre_grammaire terme gram =
    match gram with
    | [] -> []
    | Prod(nt, droite)::tail when (List.mem terme droite) -> (Prod(nt, retirer_terme terme droite))::(retirer_lettre_grammaire terme tail)
    | head::tail -> head::(retirer_lettre_grammaire terme tail)
;;


(*
 * Cas 2.
 * Rajoute les regles necessaires lorsque l'on a supprime une epsilon-regle.
 * Par exemple pour la regle S -> aSTbScS, avec la
 * suppression de S -> epsilon, nous ajouterons les regles,  :
 * S -> aTBScS
 * S -> aSTBcS
 * S -> aSTBSc
 * S -> aTBcS
 * S -> aTBSc
 * S -> aSTBc
 * S -> aTBc
 *)
let dupliquer terme regle =
    match regle with
    | Prod(nt, droite) ->   let rec dupliquer_rec positions regle =
                                match positions with
                                | [] -> [Prod(nt, droite)]
                                | head::tail -> Prod(nt, (retirer_indices head droite))::(dupliquer_rec tail regle)
                            in dupliquer_rec (toutes_combinaisons (positions_valeurs terme droite)) regle
;;


(*
 * Retire un symbole 'terme' d'une regle 'production'.
 *)
let rec retirer_lettre_production terme production =
    match production with
    | Prod(nt, droite) -> Prod(nt, retirer_terme terme droite)
    ;;


(*
 * Retire d'une grammaire 'gram' les regles qui ne produisent rien,
 * c'est-a-dire de la forme 'Prod(nt, [])'.
 *)
let rec retirer_production_vide gram =
    match gram with
    | [] -> []
    | Prod(nt, [])::tail -> retirer_production_vide tail
    | head::tail -> head::(retirer_production_vide tail)
    ;;


(*
 * Retire l'epsilon-regle associe terme 'terme'
 * (qui se trouve a gauche dans l'epsilon-regle) de la grammaire 'gram',
 * en indiquant le cas 1 (true) ou 2 (false) avec 'eps_seul'.
 * Si on est dans le cas 2, on ajoute les regles necessaires avec 'dupliquer'.
 *)
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


(*
 * Retire toutes les epsilon-regles de la grammaire 'gram'
 *)
let supprimer_toutes_epsilon_regles gram =
    let rec supprimer_toutes_epsilon_regles_rec ntpe gram =
        match ntpe with
        | [] -> gram
        | head::tail -> supprimer_toutes_epsilon_regles_rec tail (epsilon_iteration head gram)
    in list_to_set (retirer_production_vide (supprimer_toutes_epsilon_regles_rec (non_terminaux_produisent_epsilon gram) gram))
;;
