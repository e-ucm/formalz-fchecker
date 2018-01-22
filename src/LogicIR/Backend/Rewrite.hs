module LogicIR.Backend.Rewrite (replaceQuantifiers) where

import LogicIR.Expr
import LogicIR.Fold
import LogicIR.Backend.Pretty


-- Expands quantifiers in given LExpr and returns a quantifier free LExpr.
replaceQuantifiers :: LExpr -> LExpr
replaceQuantifiers = foldLExpr replaceQuantifiersAlgebra

type Env = Int

replaceQuantifiersEnvAlgebra :: LExprAlgebra (Env -> LExpr)
replaceQuantifiersEnvAlgebra = (cnst, var, uni, bin, iff, quant, arr, snull, len)
    where cnst c        = \env -> LConst c
          uni op e      = \env -> LUnop op (e env)
          bin e1 op e2  = \env -> LBinop (e1 env) op (e2 env)
          iff ge e1 e2  = \env -> LIf (ge env) (e1 env) (e2 env)
          var v         = \env -> LVar v
          quant t v d e = \env -> replaceQuantifier t v (d (env*2)) (e (env*2+1))
          arr v e       = \env -> LArray v (e env)
          snull v       = \env -> LIsnull v
          len v         = \env -> LLen v

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
