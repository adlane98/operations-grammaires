#use "type.ml";;

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
        | Prod(nt, liste) -> match liste with
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


let rec non_terminaux_produits production =
    match production with
    | Prod(nt, h::t) -> begin
        match h with
        | NT(x) -> (NT(x))::(non_terminaux_produits (Prod(nt, t)))
        | _ -> non_terminaux_produits (Prod(nt, t))
        end
    | _ -> []
    ;;


let rec regles_restantes nonterminaux grammaire =
    match grammaire with
    | [] -> []
    | Prod(nt, liste)::tail -> if (List.mem nt nonterminaux)
                               then regles_restantes nonterminaux tail
                               else Prod(nt, liste)::(regles_restantes nonterminaux tail)
    ;;


let rec subset sub set =
    match sub with
    | [] -> true
    | h::t -> if (List.mem h set)
              then subset t set
              else false
    ;;


let non_terminaux_productibles grammaire =
    let rec non_terminaux_productibles_rec grammaire regles prec acc =
        match regles with
        | [] -> let reste = (regles_restantes acc grammaire) in begin
                match reste with
                | [] -> acc
                | _ -> if (reste = prec)
                       then acc
                       else non_terminaux_productibles_rec grammaire reste regles acc
                end
        | Prod(nt, liste)::suite -> let non_terminaux = non_terminaux_produits (Prod(nt, liste)) in
                if (subset non_terminaux acc)
                then non_terminaux_productibles_rec grammaire suite [] (nt::acc)
                else non_terminaux_productibles_rec grammaire suite [] acc
    in non_terminaux_productibles_rec grammaire grammaire [] []
    ;;
