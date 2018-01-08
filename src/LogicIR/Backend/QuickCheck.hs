module LogicIR.Backend.QuickCheck () where

import LogicIR.Expr
import LogicIR.Fold
import LogicIR.Eval
import Test.QuickCheck
import Data.Maybe
import Data.List (find)
import Control.Exception

{- 

This module checks whether two LExprs are equal in the following manner:

1. collect all currently known bounds of all variables in both LExprs
2. 

-}

---- Tells, given a formula and a list of ranges for all variables in the formula,
---- whether the formula evaluates to true or to false.
--check :: LExpr -> [(Var, LExpr, LExpr)] -> LExpr
--check e []     = e
--check e varBounds = do
--    let purelyBoundedVar = fstPureBounds varBounds
--    let t@(v, mn, mx)    = if isJust purelyBoundedVar
--                           then fromJust purelyBoundedVar
--                           else head varBounds
--    value <- choose (mn,mx)
--    where randomVal       = 1
--          cnst _          = []
--          var v           = [v]
--          uni _ a         = a
--          bin a1 _ a2     = a1 ++ a2
--          iff a1 a2 a3    = a1 ++ a2 ++ a3
--          quant _ _ a1 a2 = a1 ++ a2
--          arr v a         = v : a
--          snull v         = [v]
--          len v           = [v]

-- Returns the first tuple in the given list that has pure bounds, i.e.
-- both the minimum and maximum contain no variables.
--fstPurelyBounded :: [(Var, LExpr, LExpr)] -> Maybe (Var, LExpr, LExpr)
--fstPurelyBounded ranges = find pureBound ranges
--    where pureBound (_, mn, mx) = (evalPossible mn) && (evalPossible mx)


--getInstance :: (Var, LExpr, LExpr) -> LExpr
--getInstance ((Var t s), mn, mx)

-- Replaces all occurences in the secon argument of the first tuple argument
-- with the second tuple argument, e.g. 
-- substitute ("x", 5) "x + y" = "5 + y"
--substitute :: (LExpr, LExpr) -> LExpr -> Bool
--substitute t e = foldLExpr (substituteAlgebra t)

--substituteAlgebra :: (LExpr, LExpr) -> LExprAlgebra Bool
--substituteAlgebra (old, new) = (cnst, var, uni, bin, iff, quant, arr, snull, len)
--    where cnst c           = subst (LConst c)
--          uni op e         = LUnop op (subst e)
--          bin le op re     = LBinop (subst le) op (subst re)
--          iff ge e1 e2     = LIf (subst ge) (subst e1) (subst e2)
--          var v            = subst (LVar v)
--          quant op v a1 a2 = LQuant op (subst (LVar v)) (subst a1) (subst a2)
--          arr v a          = subst (LArray (subst (LVar v)) (subst a))
--          snull v          = subst (LVar v)
--          len v            = subst (LVar v)

--          subVar v         = if (LVar v) == old then new else (LVar v)
--          subst e         = if e == old then new else e

---- Returns the minimum and maximum range, expressed in LExprs, for
---- every LVar in the given LExpr
--findRanges :: LExpr -> [Var] -> [(LExpr, LExpr, LExpr)]
--findRanges e = []

---- Returns a list with all LVar's that appear in a given LExpr.
--variables :: LExpr -> [Var]
--variables = foldLExpr (cnst, var, uni, bin, iff, quant, arr, snull, len)
--    where cnst _          = []
--          var v           = [v]
--          uni _ a         = a
--          bin a1 _ a2     = a1 ++ a2
--          iff a1 a2 a3    = a1 ++ a2 ++ a3
--          quant _ _ a1 a2 = a1 ++ a2
--          arr v a         = v : a
--          snull v         = [v]
--          len v           = [v]