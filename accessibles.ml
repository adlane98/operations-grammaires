#use "utils.ml";;


(*
 * Recupere tous les non terminaux
 * qui sont produits par le symbole 'terme' apres une seule dérivation
 * dans la liste de regles 'gram' (une grammaire).
 *)
let rec non_terminaux_accessibles_direct terme gram =
    match gram with
    | [] -> []
    | Prod(nt, droite)::tail -> if nt = terme
                                then
                                    list_to_set (
                                    recuperer_non_terminaux_regle (Prod(nt, droite))
                                    @
                                    (non_terminaux_accessibles_direct terme tail)
                                    )
                                else
                                    (non_terminaux_accessibles_direct terme tail)
;;


(*
 * Recupere les non-terminaux accessibles a
 * partir d'un non terminal 'depart' dans une
 * grammaire, une liste de regles, 'gram'.
 *)
let non_terminaux_accessibles gram depart =
    let rec non_terminaux_accessibles_rec gram parcours alphabet =
        match parcours with
        | [] -> []
        | head::tail -> let acc = (non_terminaux_accessibles_direct head gram) in
                            if (List.mem head alphabet)
                            then
                                list_to_set (
                                acc
                                @
                                non_terminaux_accessibles_rec gram (tail @ acc) (retirer_terme head alphabet) )
                            else
                                non_terminaux_accessibles_rec gram tail alphabet
    in non_terminaux_accessibles_rec gram [depart] (recuperer_non_terminaux_grammaire gram)
;;
