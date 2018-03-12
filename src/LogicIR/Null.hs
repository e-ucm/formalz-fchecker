module LogicIR.Null (lExprPreprocessNull) where

import LogicIR.Expr
import LogicIR.Fold

-- | Replace all instances of "a ==/!= null" with (!)isNull(a).
lExprPreprocessNull :: LExpr -> LExpr
lExprPreprocessNull =
  foldLExpr (defLAlgebra {bin = fbin})
  where
    fbin x o y =
      case o of
        CEqual -> nullCheck x y
        _      -> LBinop x o y
    nullCheck (LVar x@(Var (TArray _) _)) (LConst CNil) = LIsnull x
    nullCheck (LConst CNil) (LVar x@(Var (TArray _) _)) = LIsnull x
    nullCheck x y                                       = LBinop x CEqual y
