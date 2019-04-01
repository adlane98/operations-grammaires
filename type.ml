type epsilon = Epsilon ;;
type terminal = T of char ;; (* Ptet rajoute ici epsilon *)
type nonTerminal = NT of char ;;
type lettre = T of char | NT of char | Epsilon ;;
type regle = Prod of nonTerminal * (lettre list) ;;

(* Verifie si a partir de la regle 'r' le symbole 's' est accessible *)
let rec estAccessibleDirect r s =
    match r with
    | Prod(_, []) -> false
    | Prod(t, l::tail) ->   if s == l then
                                true
                            else estAccessibleDirect (Prod(t, tail)) s
    ;;

(* let rec accessibles regles l =
    match regles with
    | [] -> []
    | r::tail -> *)


let grammaireTest = [
Prod(NT('A'),    [NT('B'), T('c')]),
Prod(NT('B'),   [T('c'), T('c'), NT('D')]),
Prod(NT('B'),   [T('d')]),
Prod(NT('D'),   [NT('E')]),
Prod(NT('F'),   [T('g'), T('a')])
];;
