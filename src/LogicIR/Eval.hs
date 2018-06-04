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

evalPossibleAlgebra :: LAlgebra Bool
evalPossibleAlgebra = LAlgebra fcns fvar funi fbin fiff fqnt farr fnll flen
    where fcns _                  = True
          funi _ c                = c
          fbin le _ re            = le && re
          fiff ge e1 e2           = ge && e1 && e2
          fvar _                  = False
          fqnt _ _ a1 a2          = a1 && a2
          farr _ _                = False
          fnll (Var (TPrim _) _)  = True -- Primitive type is never null.
          fnll (Var (TArray _) _) = False
          flen _                  = True -- This possibly needs to be changed. See evalAlgebra

evalAlgebra :: LAlgebra LConst
evalAlgebra = LAlgebra fcns fvar funi fbin fiff fqnt farr fnll flen
    where fcns = id
          funi NNeg (CInt i)  = CInt (-i)
          funi LNot (CBool x) = CBool (not x)
          funi _ _            = error "evalAlgebra: invalid argument to funi"
          fbin                = evalBin
          fiff ge e1 e2       = if ge == CBool True then e1 else e2
          -- This possibly needs to be changed.
          flen _                  = CInt  10 -- error "Array length cannot yet be evaluated."
          fnll (Var (TPrim _) _)  = CBool False -- Primitive type is never null.
          fnll (Var (TArray _) _) = error "You can't call eval on an LExpr that has an (LIsnull someArrayVar) that wasn't converted to a boolean first."
          -- Things that should never happen.
          fqnt _ _ _ _ = error "Quantifiers cannot be evaluated and should be replaced using LogicIR.Rewrite.replaceQuantifiers."
          farr _ _     = error "You can't call eval on an LExpr that contains uninstantiated arrays."
          fvar _       = error "You can't call eval on an LExpr that contains uninstantiated vars."

compOps :: [LBinop]
compOps = [CLess, CEqual, CGreater]
compOpF :: Ord a => LBinop -> a -> a -> Bool
compOpF CLess    = (<)
compOpF CEqual   = (==)
compOpF CGreater = (>)
compOpF _        = error "compOpF: invalid binary operator"

val :: LConst -> Double
val (CInt  x) = fromIntegral x
val (CReal x) = x
val _         = error "val: invalid constant"

-- Evaluates a binary operator expression.
evalBin :: LConst -> LBinop -> LConst -> LConst
-- Logical operators
evalBin (CBool x) LAnd  (CBool y) = CBool (x && y)
evalBin (CBool x) LOr   (CBool y) = CBool (x || y)
evalBin (CBool x) LImpl (CBool y) = CBool (not x || (x && y))
evalBin (CBool x) CEqual (CBool y) = CBool (x == y)
-- Comparison operators
evalBin (CInt  x) o (CInt  y)
     | o `elem` compOps     = CBool $ compOpF o x y
     | o == NDiv            = CInt $ x `quot` y
     | o == NRem            = CInt $ x `rem` y
     | otherwise            = CInt $ convert o x y
evalBin (CReal x) o (CReal y)
     | o `elem` compOps = CBool $ compOpF o x y
     | otherwise        = CReal $ convertR o x y
-- Numerical operators
evalBin x o y
     | o `elem` compOps = CBool $ compOpF o (val x) (val y)
     | otherwise        = CReal $ convertR o (val x) (val y)

convertR :: (Show a, Fractional a, Real a) => LBinop -> a -> a -> a
convertR NRem x y = mod' x y
convertR NDiv x y = (/)  x y
convertR o    x y = convert o x y

convert :: (Show a, Num a) => LBinop -> a -> a -> a
convert NAdd x y = (+) x y
convert NSub x y = (-) x y
convert NMul x y = (*) x y
convert o    x y = error $ show o ++ " on " ++ show x ++ " and " ++ show y ++ " is impossible."

-- Mod that works with (negative) reals the same way
-- Java would work with them, but with arbitrary precision
-- instead of the bad precision that Java has.
mod' :: (Real t, Fractional a) => t -> t -> a
mod' x y =
  let x' = toRational x
      y' = toRational y
  in  fromRational $ x' - (y' * toRational (truncate (x' / y') :: Integer))
