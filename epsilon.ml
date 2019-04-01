#use "type.ml"
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

let rec non_terminaux_produisent_epsilon gram =
    match gram with
    | [] -> []
    | Prod(nt, [Epsilon])::tail -> nt::(non_terminaux_produisent_epsilon tail)
    | Prod(nt, _)::tail -> (non_terminaux_produisent_epsilon tail)
;;

(* TODO
let rec dupliquer terme production =
    match production with
    | [] -> []
    | head::tail ->
*)

let rec epsilon_iteration terme gram =
    match gram with
    | [] -> []
    | Prod(nt, [Epsilon])::tail when nt = terme -> epsilon_iteration terme tail
    | Prod(nt, droite)::tail when nt = terme -> (dupliquer terme Prod(nt, droite))
                                            @
                                            (epsilon_iteration terme tail)
    | head::tail -> head::(epsilon_iteration terme tail)
;;

(* TODO
let rec supprimer_epilon_regle gram ntpe =
    match ntpe with
    | [] -> []
    | head::tail ->
*)
