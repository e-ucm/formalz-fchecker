module LogicIR.Preprocess (preprocess) where

import LogicIR.Expr
import LogicIR.Fold

-- | Apply several pre-processing transformations to an 'LExpr'.
preprocess :: LExpr -> LExpr
preprocess = divisionByZero . nullChecks

-- | Replace all instances of "a ==/!= null" with (!)isNull(a).
nullChecks :: LExpr -> LExpr
nullChecks = foldLExpr (defLAlgebra {bin = fbin})
  where
    fbin x o y =
      case o of
        CEqual -> nullCheck x y
        _      -> LBinop x o y

    nullCheck (LVar x@(Var (TArray _) _)) (LConst CNil) = LIsnull x
    nullCheck (LConst CNil) (LVar x@(Var (TArray _) _)) = LIsnull x
    nullCheck x y                                       = LBinop x CEqual y

-- | Add '!= 0' for all denominators.
divisionByZero :: LExpr -> LExpr
divisionByZero =
  topLevel . foldLExpr (defLAlgebra {qnt = fqnt})
  where
    topLevel e = assertNonZero (getDenominators e) e
    fqnt o x d a =
      let denoms = filter (containsVar x) (getDenominators a)
      in  LQuant o x d (assertNonZero denoms a)

-- | Makes sure several expressions are non-zero.
assertNonZero :: [LExpr] -> LExpr -> LExpr
assertNonZero [] e = e
assertNonZero xs e = foldr1 (.&&) (map (\x -> x .!= n 0) xs) .&& e

-- | Checks whether the given variable appears in an expression.
containsVar :: Var -> LExpr -> Bool
containsVar x lexp =
  let contains = containsVar x
  in case lexp of
    LConst _        -> False
    LVar x'         -> x' == x
    LUnop _ e       -> contains e
    LBinop e _ e'   -> contains e || contains e'
    LIf c e e'      -> contains c || contains e || contains e'
    LQuant _ x' d e -> (x' /= x) && (contains d || contains e)
    LArray x' e     -> x' == x || contains e
    LIsnull x'      -> x' == x
    LLen x'         -> x' == x

-- | Get all denominators that do not contain scoped variables.
getDenominators :: LExpr -> [LExpr]
getDenominators = go []
  where
    go scoped lexp =
      case lexp of
        LUnop _ e        -> go scoped e
        LBinop e NDiv e' -> go scoped e ++ [e' | all (\sc -> not $ containsVar sc e') scoped]
        LBinop e _ e'    -> go scoped e ++ go scoped e'
        LIf c e e'       -> go scoped c ++ go scoped e ++ go scoped e'
        LQuant _ x' d e  -> go scoped d ++ go (x' : scoped) e
        LArray _ e       -> go scoped e
        _                -> []
