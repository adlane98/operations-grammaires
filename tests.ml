#use "epsilon.ml";;

let grammaireTest = [
    Prod(NT('A'),   [NT('B'); T('c')]);
    Prod(NT('B'),   [T('c'); T('c'); NT('D')]);
    Prod(NT('B'),   [T('d')]);
    Prod(NT('D'),   [NT('E')]);
    Prod(NT('F'),   [T('g'); T('a')])
];;
