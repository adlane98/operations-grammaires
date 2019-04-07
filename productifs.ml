#use "accessibles.ml";;


(*
 * Renvoie toutes les regles d'une grammaire dont le membre gauche ne fait pas
 * partie d'une liste de non-terminaux donnés
 * @param une grammaire et une liste de non-terminaux
 * @return une liste de productions
 *)
let rec regles_restantes nonterminaux grammaire =
    match grammaire with
    | [] -> []
    | Prod(nt, liste)::tail -> if (List.mem nt nonterminaux)
                               then regles_restantes nonterminaux tail
                               else Prod(nt, liste)::(regles_restantes nonterminaux tail)
    ;;


(*
 * Recupere tous les non-terminaux productifs d'une grammaire
 * @param une grammaire
 * @return une liste de non-terminaux
 *)
let non_terminaux_productifs grammaire =
    let rec non_terminaux_productifs_rec grammaire regles prec acc =
        match regles with
        | Prod(nt, liste)::suite -> let non_terminaux = non_terminaux_produits (Prod(nt, liste)) in
                if ((not (List.mem nt acc)) && subset non_terminaux acc)
                then non_terminaux_productifs_rec grammaire suite prec (nt::acc)
                else non_terminaux_productifs_rec grammaire suite prec acc
        | [] -> let reste = (regles_restantes acc grammaire) in begin
                match reste with
                | [] -> acc
                | reste -> if (subset prec reste)
                           then acc
                           else non_terminaux_productifs_rec grammaire reste regles acc
                end
    in non_terminaux_productifs_rec grammaire grammaire grammaire []
    ;;
