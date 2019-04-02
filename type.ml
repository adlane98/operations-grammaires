type lettre = T of char | NT of char | Epsilon ;;
type regle = Prod of (lettre * (lettre list)) ;;
type grammaire = regle list;;

(* Verifie si a partir de la regle 'r' le symbole 's' est accessible *)
let rec est_accessible_direct regle symbole =
    match regle with
    | Prod(_, []) -> false
    | Prod(nt, head::tail) -> if (symbole == head)
                              then true
                              else est_accessible_direct (Prod(nt, tail)) symbole
    ;;

let grammaireTest = [
Prod(NT('A'),   [NT('B'); T('c')]);
Prod(NT('B'),   [T('c'); T('c'); NT('D')]);
Prod(NT('B'),   [T('d')]);
Prod(NT('D'),   [NT('E')]);
Prod(NT('F'),   [T('g'); T('a')])
];;
