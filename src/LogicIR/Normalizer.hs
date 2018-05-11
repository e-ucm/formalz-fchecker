{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators  #-}
module LogicIR.Normalizer
      ( toCNF
      , toNNF
      , skolemize
      , distribute
      , fixpoint, (>>*)
      ) where

import Control.Arrow ((>>>))

import LogicIR.Expr
import LogicIR.Fold
import LogicIR.Parser      ()
import LogicIR.TypeChecker (typeOf)

-- TODO rename variables
-- TODO normalize inner arithmetic expressions

-- | Convert a logic formula to Conjunctive Normal Form (CNF).
toCNF :: LExpr -> LExpr
toCNF = toNNF >>* skolemize >>* distribute >>* domainCNF

-- | Convert a logic formula to Negation Normal Form (NNF).
toNNF :: LExpr -> LExpr
toNNF = elimEquiv >>* elimImpl >>* notInwards
  where
    elimEquiv :: LExpr -> LExpr
    elimEquiv = foldLExpr (defLAlgebra {bin = fBin})
      where fBin x op y
              | op == CEqual && typeOf x == Right "bool" =
                  (x .==> y) .&& (y .==> x)
              | otherwise = LBinop x op y
    elimImpl :: LExpr -> LExpr
    elimImpl = foldLExpr (defLAlgebra {bin = fBin})
      where fBin x LImpl y = lnot x .|| y
            fBin x o y     = LBinop x o y
    notInwards :: LExpr -> LExpr
    notInwards = foldLExpr (defLAlgebra {uni = fUni})
      where fUni LNot (LBinop x LOr y)    = lnot x .&& lnot y
            fUni LNot (LBinop x LAnd y)   = lnot x .|| lnot y
            fUni LNot (LUnop LNot y)      = y
            fUni LNot (LQuant QAll x d a) = exists x d (lnot a)
            fUni LNot (LQuant QAny x d a) = forall x d (lnot a)
            fUni o x                      = LUnop o x

-- | Quantifier elimination.
skolemize :: LExpr -> LExpr
skolemize = foldLExpr (defLAlgebra {bin = fBin})
  where fBin p LAnd (LQuant o x d a) = LQuant o x d (p .&& a)
        fBin p LOr  (LQuant o x d a) = LQuant o x d (p .|| a)
        fBin x o y                   = LBinop x o y

-- | Rewrite the rule `a \/ (b /\ c) ~> (a \/ b) /\ (a \/ c)`.
distribute :: LExpr -> LExpr
distribute = foldLExpr (defLAlgebra {bin = fBin})
  where fBin p LOr (LBinop x LAnd y) = (p .|| x) .&& (p .|| y)
        fBin x o y                   = LBinop x o y

-- | Compose two transformations (up to fixpoint).
(>>*) :: (LExpr -> LExpr) -> (LExpr -> LExpr) -> (LExpr -> LExpr)
f >>* g = fixpoint f >>> fixpoint g

-- | Converge to fixpoint with given initial value.
fixpoint :: (LExpr -> LExpr) -> LExpr -> LExpr
fixpoint k l = let l' = k l
               in  if l == l' then l else k l'

domainCNF :: LExpr -> LExpr
domainCNF = foldLExpr (defLAlgebra {qnt = fQnt})
  where fQnt o x d = LQuant o x (toCNF d)
