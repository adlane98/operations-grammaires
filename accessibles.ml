#use "type.ml"
#use "recupGrammaire.ml"


let rec non_terminaux_accessibles_direct terme grammaire =
    match grammaire with
    | [] -> []
    | Prod(nt, l)::tail -> if nt = terme
                           then list_to_set (
                                                (recuperer_non_terminaux_regle (Prod(nt, l))) @
                                                (non_terminaux_accessibles_direct terme tail)
                                            )
                           else non_terminaux_accessibles_direct terme tail
    ;;

let rec non_terminaux_accessibles grammaire parcours alphabet =
    match parcours with
    | [] -> []
    | head::tail -> if (List.mem head alphabet)
                    then let acc = (non_terminaux_accessibles_direct head grammaire) in
                             list_to_set (
                                             (acc) @
                                             (non_terminaux_accessibles grammaire (tail @ acc) (retirer_terme head alphabet))
                                         )
                    else non_terminaux_accessibles grammaire tail alphabet
    ;;




let alphabet = recuperer_non_terminaux_grammaire grammaireTest;;
non_terminaux_accessibles grammaireTest [NT('F')] alphabet;;
