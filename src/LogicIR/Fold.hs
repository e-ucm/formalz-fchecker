module LogicIR.Fold where

import LogicIR.Expr

-- Fold algrbra for numeral expressions
type NExprAlgebra r = (Int -> r, -- NConst
                       String -> r, -- NVar
                       NUnop -> r -> r, -- NUnop
                       r -> NBinop -> r -> r, -- NBinop
                       String -> r -> r -- NArray
                      )

-- Fold for numeral expressions
foldNExpr :: NExprAlgebra r -> NExpr -> r
foldNExpr (fConst, fVar, fUnop, fBinop, fArray) = fold where
    fold e = case e of
                  NConst n -> fConst n
                  NVar n -> fVar n
                  NUnop o e -> fUnop o (fold e)
                  NBinop a o b -> fBinop (fold a) o (fold b)
                  NArray n e -> fArray n (fold e)

-- Fold algebra for logical expressions
type LExprAlgebra r = (Bool -> r, -- LConst
                       String -> r, -- LVar
                       r -> r, -- LNot
                       r -> LBinop -> r -> r, -- LBinop
                       NExpr -> COp -> NExpr -> r, -- LComp
                       QOp -> [String] -> r -> r, -- LQuant
                       String -> NExpr -> r -- LArray
                      )

-- Fold for logical expressions
foldLExpr :: LExprAlgebra r -> LExpr -> r
foldLExpr (fConst, fVar, fNot, fBinop, fComp, fQuant, fArray) = fold where
    fold e = case e of
                  LConst c -> fConst c
                  LVar n -> fVar n
                  LNot e -> fNot (fold e)
                  LBinop a o b -> fBinop (fold a) o (fold b)
                  LComp a o b -> fComp a o b
                  LQuant o vs e -> fQuant o vs (fold e)
                  LArray n e -> fArray n e