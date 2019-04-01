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

(*
let rec non_terminaux_accessibles lettre grammaire parcours =
    match grammaire with
    | Prod(nt, production)::tail -> if lettre = nt
                                    then *)


let rec non_terminaux_productibles nonterminal grammaire parcourus =
    let regles = recuperer_regles nonterminal grammaire in
        match regles with
        | [] -> []
        | head::tail -> let nonterminaux = recuperer_non_terminaux_regle head in
                            match nonterminaux with
                            | [] -> 
