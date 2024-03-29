#use "epsilon2.ml";;

let grammaireTest1 = [
    Prod(NT('Z'),   [Epsilon]);
    Prod(NT('A'),   [NT('B'); T('c')]);
    Prod(NT('B'),   [T('c'); T('c'); NT('D')]);
    Prod(NT('B'),   [T('d')]);
    Prod(NT('D'),   [NT('E')]);
    Prod(NT('F'),   [T('g'); T('a')])
];;

let grammaireTest2 = [
    Prod(NT('S'),   [T('a'); NT('S'); NT('B'); NT('S'); T('b'); T('d'); NT('S')]);
    Prod(NT('A'),   [T('c'); NT('S')]);
    Prod(NT('S'),   [NT('S')]);
    Prod(NT('B'),   [NT('S')]);
    Prod(NT('B'),   [Epsilon]);
    Prod(NT('S'),   [Epsilon]);
    Prod(NT('D'),   [NT('E')]);
    Prod(NT('D'),   [Epsilon]);
    Prod(NT('F'),   [T('g'); T('a')])
];;

let grammaireTest3 = [
    Prod(NT('U'),   [NT('T'); T('a'); NT('U')]);
    Prod(NT('U'),   [NT('T'); T('a'); NT('T')]);
    Prod(NT('V'),   [NT('T'); T('b'); NT('V')]);
    Prod(NT('V'),   [NT('T'); T('b'); NT('T')]);
    Prod(NT('S'),   [NT('U')]);
    Prod(NT('S'),   [NT('V')]);
    Prod(NT('T'),   [T('a'); NT('T'); T('b'); NT('T')]);
    Prod(NT('T'),   [T('b'); NT('T'); T('a'); NT('T')]);
    Prod(NT('T'),   [Epsilon]);
];;

let grammaireEpsilon = [
    Prod(NT('S'),   [Epsilon])
];;
