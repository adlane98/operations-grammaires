#use "productifs.ml";;

(* Fichier de la deuxieme methode *)

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
 * Remplace toutes les regles de la forme 'Prod(x, [])' par 'Prod(x, [Epsilon])'.
 *)
let rec remplacer_vide_epsilon gram =
    let remplacer (Prod(nt, droite)) =
        if droite = []
        then (Prod(nt, [Epsilon]))
        else (Prod(nt, droite))
    in List.map remplacer gram
;;

(*
 * Supprime les regles de la forme 'Prod(terme, [Epsilon])'.
 *)
let rec supprimer_epsilon_regle terme gram =
    match gram with
    | Prod(nt, [Epsilon])::tail when nt = terme -> supprimer_epsilon_regle terme tail
    | head::tail -> head::(supprimer_epsilon_regle terme tail)
    | [] -> []
;;

(*
 * Supprime les regles de la forme 'Prod(terme, [terme])'.
 *)
let rec nettoyer gram =
    match gram with
    | Prod(nt, liste)::tail when liste = [nt] -> nettoyer tail
    | Prod(nt, liste)::tail -> (Prod(nt, liste))::(nettoyer tail)
    | [] -> []
;;

(*
 * Renvoie vrai s'il existe une epsilon regle dans la grammaire gram.
 *)
let rec existe_epsilon_regle gram =
    match gram with
    | Prod(_, [Epsilon])::_ -> true
    | _::tail -> existe_epsilon_regle tail
    | [] -> false
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
let dupliquer2 terme gram =
    let rec dupliquer_rec2 terme gram nouvelle_gram =
        match gram with
        | [] -> remplacer_vide_epsilon nouvelle_gram
        | Prod(nt, liste)::suite when (List.mem terme liste) ->
            let nouvelle_regle =
                let positions = positions_valeurs terme liste in
                    let rec regle_a_ajouter pos =
                        match pos with
                        | [] -> liste
                        | head::tail ->
                            let nouvelle_liste = retirer_indice head liste in
                                if List.mem (Prod(nt, nouvelle_liste)) nouvelle_gram
                                then regle_a_ajouter tail
                                else nouvelle_liste
                    in Prod(nt, (regle_a_ajouter positions))
            in
                if nouvelle_regle = (Prod(nt, liste))
                then dupliquer_rec2 terme suite nouvelle_gram
                else dupliquer_rec2 terme (nouvelle_regle::gram) (nouvelle_regle::nouvelle_gram)
        | _::suite -> dupliquer_rec2 terme suite nouvelle_gram
    in dupliquer_rec2 terme gram gram
;;

let rec epsilon_iteration2 (terme, eps_seul) gram =
    let gram_sans_epsilon = supprimer_epsilon_regle terme gram in
        if eps_seul
        then retirer_lettre_grammaire terme gram_sans_epsilon
        else dupliquer2 terme gram_sans_epsilon
;;


let rec supprimer_toutes_epsilon_regles2 gram =
    let resultat =
        let rec supprimer_toutes_epsilon_regles_rec2 ntpe gram =
            match ntpe with
            | [] -> gram
            | head::tail -> supprimer_toutes_epsilon_regles_rec2 tail (epsilon_iteration2 head gram)
        in supprimer_toutes_epsilon_regles_rec2 (non_terminaux_produisent_epsilon gram) (nettoyer gram)
    in
        if (existe_epsilon_regle resultat)
        then supprimer_toutes_epsilon_regles2 resultat
        else resultat
;;
