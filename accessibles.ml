#use "type.ml"
#use "recupGrammaire.ml"


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

let rec non_terminaux_accessibles gram parcours alphabet =
    match parcours with
    | [] -> []
    | head::tail -> let acc = (non_terminaux_accessibles_direct head gram) in
                        if (List.mem head alphabet)
                        then
                            list_to_set (
                            acc
                            @
                            non_terminaux_accessibles gram (tail @ acc) (retirer_terme head alphabet) )
                        else
                            non_terminaux_accessibles gram tail alphabet
;;

let alphabet = recuperer_non_terminaux_grammaire grammaireTest;;
non_terminaux_accessibles grammaireTest [NT('F')] alphabet;;
