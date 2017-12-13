module LogicIR.Backend.Null (lExprPreprocessNull) where

import           LogicIR.Expr
import           LogicIR.Fold

-- | Replace all instances of "a ==/!= null" with (!)isNull(a).
lExprPreprocessNull :: LExpr -> LExpr
lExprPreprocessNull =
  foldLExpr (LConst, LVar, LUnop, fBinop, LIf, LQuant, LArray, LIsnull, LLen)
  where
    fBinop a o b =
      case o of
        CEqual -> nullCheck a b
        _      -> LBinop a o b
    nullCheck (LVar v@(Var (TArray _) _)) (LConst CNil) = LIsnull v
    nullCheck (LConst CNil) (LVar v@(Var (TArray _) _)) = LIsnull v
    nullCheck a b                                       = LBinop a CEqual b
