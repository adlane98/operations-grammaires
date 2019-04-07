#use "tests.ml";;

let accTest1 = non_terminaux_accessibles grammaireTest1 (NT('A'));;
let accTest2 = non_terminaux_accessibles grammaireTest1 (NT('F'));;
let accTest3 = non_terminaux_accessibles grammaireTest2 (NT('S'));;
let accTest4 = non_terminaux_accessibles grammaireTest3 (NT('U'));;

let prodTest1 = regles_productives grammaireTest1;;
let prodTest2 = regles_productives grammaireTest2;;
let prodTest3 = regles_productives grammaireTest3;;
let prodTestEpsilon = regles_productives grammaireEpsilon;;

let termesProdTest1 = non_terminaux_productifs grammaireTest1;;
let termesProdTest2 = non_terminaux_productifs grammaireTest2;;
let termesProdTest3 = non_terminaux_productifs grammaireTest3;;
let termesProdTestEpsilon = non_terminaux_productifs grammaireEpsilon;;

let epsTest1 = supprimer_toutes_epsilon_regles2 grammaireTest1;;
let epsTest2 = supprimer_toutes_epsilon_regles2 grammaireTest2;;
let epsTest3 = regles_productives grammaireTest3;;
let epsTestEpsilon = supprimer_toutes_epsilon_regles2 grammaireEpsilon;;
