#use "type.ml"

let rec contient x l =
    match l with
    | [] -> false
    | head::tail -> if head = x then true else contient x tail


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

let rec recuperer_non_terminaux_regle production =
    let rec recuperer_non_terminaux_regle_rec production res =
        match production with
        | Prod(nt, liste) ->    match liste with
                                | [] -> nt::res
                                | Epsilon::[] -> nt::res
                                | T(_)::tail -> recuperer_non_terminaux_regle_rec (Prod(nt, tail)) res
                                | NT(x)::tail -> recuperer_non_terminaux_regle_rec (Prod(nt, tail)) (NT(x)::res)

    in (list_to_set (recuperer_non_terminaux_regle_rec production []))
;;

let rec recuperer_non_terminaux_grammaire grammaire =
    match grammaire with
    | [] -> []
    | head::tail -> list_to_set
                    ((recuperer_non_terminaux_regle head)
                    @
                    (recuperer_non_terminaux_grammaire tail))
;;

let retirer_terme terme alphabet =
    let rec retirer_terme_rec terme alphabet res =
        match alphabet with
        | [] -> res
        | head::tail -> if head = terme
                        then retirer_terme_rec terme tail res
                        else retirer_terme_rec terme tail (head::res)
    in List.rev (retirer_terme_rec terme alphabet [])
;;


let rec nombre_occurences terme alphabet =
    match alphabet with
    | [] -> 0
    | head::tail -> if head = terme
                    then 1 + (nombre_occurences terme tail)
                    else nombre_occurences terme tail
;;

let positions_valeurs n liste =
    let rec positions_valeurs_rec n liste i =
        match liste with
        | [] -> []
        | head::tail -> if head = n
                        then i::(positions_valeurs_rec n tail (i + 1))
                        else positions_valeurs_rec n tail (i + 1)
    in positions_valeurs_rec n liste 0
;;

let rec retirer_indice n liste =
    match liste with
    | [] -> []
    | head::tail -> if n = 0
                    then tail
                    else head::(retirer_indice (n - 1) tail)
;;

let rec retirer_indices ns liste =
    match ns with
    | [] -> liste
    | head::tail -> let moinsun x = x - 1 in
                    retirer_indices (List.map moinsun tail) (retirer_indice head liste)
;;


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

let toutes_combinaisons liste =
    let rec toutes_combinaisons_rec n liste =
        match n with
        | 0 -> []
        | _ -> (toutes_combinaisons_rec (n - 1) liste) @ (combinaisons n liste)
    in toutes_combinaisons_rec (List.length liste) liste
;;

let test = combinaisons 4 [1;2;3;4;5];;
(*
let rec non_terminaux_accessibles lettre grammaire parcours =
    match grammaire with
    | Prod(nt, production)::tail -> if lettre = nt
                                    then
*)
