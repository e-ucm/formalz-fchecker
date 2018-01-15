module LogicIR.Eval (eval, evalPossible, evalIfPossible) where

import LogicIR.Expr
import LogicIR.Fold
import LogicIR.Backend.Pretty

import Data.Bits

-- Tells to which LConst a given LExpr evaluates.
eval :: LExpr -> LConst
eval = foldLExpr evalAlgebra

evalIfPossible :: LExpr -> LExpr
evalIfPossible e = if evalPossible e then (LConst (eval e)) else e

-- Returns True if an LExpr only contains constants and no variables whatsoever.
evalPossible :: LExpr -> Bool
evalPossible = foldLExpr evalPossibleAlgebra

evalPossibleAlgebra :: LExprAlgebra Bool
evalPossibleAlgebra = (cnst, var, uni, bin, iff, quant, arr, snull, len)
    where cnst _          = True
          uni _ c         = c
          bin le _ re     = le && re
          iff ge e1 e2    = ge && e1 && e2
          var v           = False
          quant _ _ a1 a2 = a1 && a2
          arr v a         = False
          snull v         = True -- This possibly needs to be changed. See evalAlgebra
          len v           = True -- This possibly needs to be changed. See evalAlgebra

evalAlgebra :: LExprAlgebra LConst
evalAlgebra = (cnst, var, uni, bin, iff, quant, arr, snull, len)
    where cnst b@(CBool _) = b
          cnst i@(CInt  _) = i

          uni NNeg (CInt i)  = CInt (-i)
          uni NNot (CInt i)  = CInt (-i - 1)
          uni LNot (CBool b) = CBool (not b)

          bin le o re     = evalBin le o re

          iff ge e1 e2    = if ge == (CBool True) then e1 else e2
          
          -- This possibly needs to be changed.
          snull v         = CBool False  -- error "Null checks cannot yet be evaluated."
          len v           = CInt  10 -- error "Array length cannot yet be evaluated."

          -- Things that should never happen.
          quant _ _ e1 e2 = error "Quantifiers cannot be evaluated and should be replaced using LogicIR.Rewrite.replaceQuantifiers."
          arr v a         = error "You can't call eval on an LExpr that contains uninstantiated arrays."
          var v           = error "You can't call eval on an LExpr that contains uninstantiated vars."

-- Evaluates a binary operator expression.
-- Comparison operators
evalBin le       CEqual   re       = CBool (le == re)
evalBin (CInt l) CLess    (CInt r) = CBool (l < r)
evalBin (CInt l) CGreater (CInt r) = CBool (l > r)
-- Logical operators
evalBin (CBool l) LAnd  (CBool r) = CBool (l && r)
evalBin (CBool l) LOr   (CBool r) = CBool (l || r)
evalBin (CBool l) LImpl (CBool r) = CBool ((not l) || (l && r))
-- Numerical operators
evalBin (CInt l) o (CInt r) = CInt ((convert o) l r)
     -- Both NShr and NShl are evaluated using the Data.Bits.shift function,
     -- where a right shift is achieved by inverting the r integer.
     where convert NAdd = (+)
           convert NSub = (-)
           convert NMul = (*)
           convert NDiv = div
           convert NRem = mod
           convert NShl = shiftL
           convert NShr = shiftR
           convert NAnd = (.&.)
           convert NOr  = (.|.)
           convert NXor = xor
