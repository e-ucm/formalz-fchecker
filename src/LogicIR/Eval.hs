module LogicIR.Eval (eval, evalPossible, evalIfPossible) where

import LogicIR.Expr
import LogicIR.Fold

-- Tells to which LConst a given LExpr evaluates.
eval :: LExpr -> LConst
eval = foldLExpr evalAlgebra

evalIfPossible :: LExpr -> LExpr
evalIfPossible e = if evalPossible e then LConst (eval e) else e

-- Returns True if an LExpr only contains constants and no variables whatsoever.
evalPossible :: LExpr -> Bool
evalPossible = foldLExpr evalPossibleAlgebra

evalPossibleAlgebra :: LExprAlgebra Bool
evalPossibleAlgebra = (cnst, var, uni, bin, iff, quant, arr, snull, len)
    where cnst _                   = True
          uni _ c                  = c
          bin le _ re              = le && re
          iff ge e1 e2             = ge && e1 && e2
          var v                    = False
          quant _ _ a1 a2          = a1 && a2
          arr v a                  = False
          snull (Var (TPrim _) _)  = True -- Primitive type is never null.
          snull (Var (TArray _) _) = False
          len v                    = True -- This possibly needs to be changed. See evalAlgebra

evalAlgebra :: LExprAlgebra LConst
evalAlgebra = (cnst, var, uni, bin, iff, quant, arr, snull, len)
    where cnst = id
          uni NNeg (CInt i)  = CInt (-i)
          uni LNot (CBool b) = CBool (not b)
          bin = evalBin
          iff ge e1 e2    = if ge == CBool True then e1 else e2
          -- This possibly needs to be changed.
          len v           = CInt  10 -- error "Array length cannot yet be evaluated."
          snull (Var (TPrim _) _)  = CBool False -- Primitive type is never null.
          snull (Var (TArray _) _) = error "You can't call eval on an LExpr that has an (LIsnull someArrayVar) that wasn't converted to a boolean first."
          -- Things that should never happen.
          quant _ _ e1 e2 = error "Quantifiers cannot be evaluated and should be replaced using LogicIR.Rewrite.replaceQuantifiers."
          arr v a         = error "You can't call eval on an LExpr that contains uninstantiated arrays."
          var v           = error "You can't call eval on an LExpr that contains uninstantiated vars."

-- Evaluates a binary operator expression.
-- Comparison operators
evalBin le       CEqual   re       = CBool (le == re)
evalBin (CInt  l) CLess    (CInt  r) = CBool (l < r)
evalBin (CReal l) CLess    (CReal r) = CBool (l < r)
evalBin (CInt  l) CGreater (CInt  r) = CBool (l > r)
evalBin (CReal l) CGreater (CReal r) = CBool (l > r)
-- Logical operators
evalBin (CBool l) LAnd  (CBool r) = CBool (l && r)
evalBin (CBool l) LOr   (CBool r) = CBool (l || r)
evalBin (CBool l) LImpl (CBool r) = CBool (not l || (l && r))
-- Numerical operators
evalBin (CInt  l) NRem (CInt  r) = CInt  (l `mod`  r)
evalBin (CInt  l) NDiv (CInt  r) = CInt  (l `quot` r)
evalBin (CReal l) NDiv (CInt  r) = CReal (l / (fromIntegral r))
evalBin (CInt  l) NDiv (CReal r) = CReal ((fromIntegral l) / r)
evalBin (CInt  l) o    (CInt  r) = CInt  (convert o l r)
evalBin (CReal l) o    (CReal r) = CReal (convert o l r)
evalBin (CInt  l) o    (CReal r) = CReal (convert o (fromIntegral l) r)
evalBin (CReal l) o    (CInt  r) = CReal (convert o l (fromIntegral r))

convert NAdd a b = (+) a b
convert NSub a b = (-) a b
convert NMul a b = (*) a b
