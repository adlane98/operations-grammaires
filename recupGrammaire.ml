let rec contient x l =
    match l with
    | [] -> false
    | head::tail -> if head = x
                    then true
                    else contient x tail
    ;;

let list_to_set liste =
    let rec list_to_set_rec liste ensemble =
        match liste with
        | [] -> ensemble
        | head::tail -> if (List.mem head ensemble)
                        then list_to_set_rec tail ensemble
                        else list_to_set_rec tail (head::ensemble)
    in list_to_set_rec liste []
    ;;

let rec recuperer_non_terminaux_regle production =
    let rec recuperer_non_terminaux_regle_rec production res =
        match production with
        | Prod(nt, liste) ->    match liste with
                                | T(_)::tail -> recuperer_non_terminaux_regle_rec (Prod(nt, tail)) res
                                | NT(x)::tail -> recuperer_non_terminaux_regle_rec (Prod(nt, tail)) (NT(x)::res)
                                | _ -> nt::res
    in (list_to_set (recuperer_non_terminaux_regle_rec production []))
    ;;

let rec recuperer_non_terminaux_grammaire grammaire =
    match grammaire with
    | [] -> []
    | head::tail -> list_to_set(
                                   (recuperer_non_terminaux_regle head) @
                                   (recuperer_non_terminaux_grammaire tail)
                               )
    ;;

let retirer_terme terme alphabet =
    let rec retirer_terme_rec terme alphabet res =
        match alphabet with
        | [] -> res
        | head::tail -> if head = terme
                        then retirer_terme_rec terme tail res
                        else retirer_terme_rec terme tail (head::res)
    in retirer_terme_rec terme alphabet []
    ;;
