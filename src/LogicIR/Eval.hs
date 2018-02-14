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

compOps = [CLess, CEqual, CGreater]
compOpF CLess    = (<)
compOpF CEqual   = (==)
compOpF CGreater = (>)

val (CInt  i) = fromIntegral i
val (CReal r) = r

-- Evaluates a binary operator expression.
-- Logical operators
evalBin (CBool l) LAnd  (CBool r) = CBool (l && r)
evalBin (CBool l) LOr   (CBool r) = CBool (l || r)
evalBin (CBool l) LImpl (CBool r) = CBool (not l || (l && r))
-- Comparison operators
evalBin (CInt  l) o (CInt  r)
     | elem o compOps       = CBool $ (compOpF o) l r
     | o == NDiv            = CInt $ l `quot` r
     | o == NRem            = CInt $ l `rem` r
     | otherwise            = CInt $ convert o l r
evalBin (CReal l) o (CReal r)
     | elem o compOps = CBool $ (compOpF o) l r
     | otherwise      = CReal $ convertR o l r
-- Numerical operators
evalBin l o r
     | elem o compOps = CBool $ (compOpF o) (val l) (val r)
     | otherwise      = CReal $ convertR o (val l) (val r)

convertR NRem a b = mod' a b
convertR NDiv a b = (/)  a b
convertR o    a b = convert o a b

convert NAdd a b = (+)  a b
convert NSub a b = (-)  a b
convert NMul a b = (*)  a b
convert o    a b = error $ (show o) ++ " on " ++ (show a) ++ " and " ++ (show b) ++ " is impossible."

-- Mod that works with (negative) reals the same way
-- Java would work with them, but with arbitrary precision
-- instead of the bad precision that Java has.
mod' x y = do
     let a = toRational x
     let b = toRational y
     let c = a - (b * (toRational $ truncate (a / b)))
     fromRational c
