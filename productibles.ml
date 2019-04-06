#use "type.ml";;

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


(* let non_terminaux_productibles grammaire = *)
    let rec non_terminaux_productibles_rec grammaire regles prec acc =
        match regles with
        | Prod(nt, liste)::suite -> let non_terminaux = non_terminaux_produits (Prod(nt, liste)) in
                if ((not (List.mem nt acc)) && subset non_terminaux acc)
                then non_terminaux_productibles_rec grammaire suite prec (nt::acc)
                else non_terminaux_productibles_rec grammaire suite prec acc
        | [] -> let reste = (regles_restantes acc grammaire) in begin
                match reste with
                | [] -> acc
                | reste -> if (subset prec reste)
                           then acc
                           else non_terminaux_productibles_rec grammaire reste regles acc
                end
    (* in non_terminaux_productibles_rec grammaire grammaire [] [] *)
    ;;

#trace non_terminaux_productibles_rec;;
