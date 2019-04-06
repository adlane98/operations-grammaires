type nonTerminal = NT of char;;
type lettre = T of char | NT of char | Epsilon ;;
type regle = Prod of (lettre * (lettre list)) ;;
type grammaire = regle list;;
