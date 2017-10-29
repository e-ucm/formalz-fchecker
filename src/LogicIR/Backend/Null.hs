module LogicIR.Backend.Null (lExprPreprocessNull) where

import LogicIR.Expr
import LogicIR.Fold

-- This module preprocesses LExpr to replace all instances of "a == null" and "a != null" with isNull(a) and !isNull(a) respectively
-- It is a bit of a hack, but the alternative is to handle this in the Java fold, which would result is far more unreadable code...

lExprPreprocessNull :: LExpr -> LExpr
lExprPreprocessNull = foldLExpr (LConst, LVar, LUnop, fBinop, LIf, LQuant, LArray, LIsnull, LLen) where
    fBinop a o b = case o of
                        CEqual -> nullCheck a b
                        CNEqual -> LUnop LNot (nullCheck a b)
                        _ -> LBinop a o b
                        where nullCheck (LVar v@(Var (TArray _) _)) (LConst CNil) = LIsnull v
                              nullCheck (LConst CNil) (LVar v@(Var (TArray _) _)) = LIsnull v
                              nullCheck a b = LBinop a CEqual b