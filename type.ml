type lettre = T of char | NT of char | Epsilon ;;
type regle = Prod of (lettre * (lettre list)) ;;
type grammaire = (regle list);;



let grammaireTest = [
Prod(NT('A'),   [NT('B'); T('c')]);
Prod(NT('B'),   [T('c'); T('c'); NT('D')]);
Prod(NT('B'),   [T('d')]);
Prod(NT('D'),   [NT('E')]);
Prod(NT('F'),   [T('g'); T('a')])
];;

let grammaireTest2 = [
Prod(NT('S'),   [T('a'); NT('S'); NT('T'); T('b'); NT('B'); T('c')]);
Prod(NT('S'),   [T('s'); NT('S')]);
Prod(NT('S'),   [Epsilon]);
Prod(NT('T'),   [T('c'); NT('B'); NT('U')]);
Prod(NT('T'),   [Epsilon]);
Prod(NT('B'),   [T('d'); NT('B'); NT('S')]);
Prod(NT('B'),   [T('d'); NT('T'); NT('U')]);
Prod(NT('U'),   [NT('U')]);
Prod(NT('B'),   [Epsilon]);
Prod(NT('U'),   [Epsilon]);
Prod(NT('B'),   [Epsilon])
];;
