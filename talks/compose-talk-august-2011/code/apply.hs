applyRule :: Term -> Term -> Term -> Maybe Term
applyRule lhs rhs t = 
  case match lhs t of
    Fail -> Fail
    Substitution sigma -> 
      case substitute sigma rhs of
        Fail -> Fail
        Term t' -> t'
