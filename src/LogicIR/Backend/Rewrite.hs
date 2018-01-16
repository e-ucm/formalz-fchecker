module LogicIR.Backend.Rewrite (replaceQuantifiers) where

import LogicIR.Expr
import LogicIR.Fold
import LogicIR.Backend.Pretty


-- Expands quantifiers in given LExpr and returns a quantifier free LExpr.
replaceQuantifiers :: LExpr -> LExpr
replaceQuantifiers = foldLExpr replaceQuantifiersAlgebra

replaceQuantifiersAlgebra :: LExprAlgebra LExpr
replaceQuantifiersAlgebra = (cnst, var, uni, bin, iff, quant, arr, snull, len)
    where cnst          = LConst
          uni           = LUnop
          bin           = LBinop
          iff           = LIf
          var           = LVar
          quant t v d e = replaceQuantifier t v d e
          arr           = LArray
          snull         = LIsnull
          len           = LLen

-- replaces an LQuant with a conjunction (if universal) or disjunction (if
-- existential). In the second argument of the con/disjunct, the quantifier Var
-- is replaced by Var'. This makes it possible to just substitute both of them
-- with a value in the end. Since Java identifiers can't contain the ' char,
-- we can safely assume that Varname ++ ' is always a fresh identifier.
replaceQuantifier :: QOp -> Var -> LExpr -> LExpr -> LExpr
replaceQuantifier op var domain e = LBinop e1 combiner e2
    where prime (Var t name) = Var t (name ++ "'")
          e1                 = LBinop domain LImpl e
          e2                 = replace var (prime var) (LBinop domain LImpl e)
          combiner           = if op == QAll then LAnd else LOr

-- Returns an LExpr where all occurences of vOld in the given LExpr are
-- replaced by vNew.
replace :: Var -> Var -> LExpr -> LExpr
replace vOld vNew = foldLExpr algebra
    where algebra :: LExprAlgebra LExpr
          algebra = (cnst, var, uni, bin, iff, quant, arr, snull, len)
          subst v = if v == vOld then vNew else v
          cnst c          = LConst c
          uni o c         = LUnop o c
          bin le o re     = LBinop le o re
          iff ge e1 e2    = LIf ge e1 e2
          var v           = LVar (subst v)
          quant t v d e   = replaceQuantifier t v d e
          arr v e         = LArray (subst v) e
          snull v         = LIsnull (subst v)
          len v           = LLen (subst v)
