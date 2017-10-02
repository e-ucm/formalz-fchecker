module LogicIR.Fold where

import LogicIR.Expr

-- Fold algebra for logical expressions
type LExprAlgebra r = (Bool -> r, -- LConst
                       Var -> r, -- LVar
                       r -> r, -- LNot
                       r -> LBinop -> r -> r, -- LBinop
                       r -> COp -> r -> r, -- LComp
                       QOp -> Var -> r -> r, -- LQuant
                       Var -> [NExpr] -> r, -- LArray
                       r, -- LNil
                       Int -> r, -- NConst
                       NUnop -> r -> r, -- NUnop
                       r -> NBinop -> r -> r, -- NBinop
                       r -> r -> r -> r, -- NIf
                       Var -> r -- NLen
                      )

-- Fold for logical expressions
foldLExpr :: LExprAlgebra r -> LExpr -> r
foldLExpr (flConst, flVar, flNot, flBinop, flComp, flQuant, flArray, flNil, fnConst, fnUnop, fnBinop, fnIf, fnLen) = fold where
    fold e = case e of
                  LConst c -> flConst c
                  LVar n -> flVar n
                  LNot e -> flNot (fold e)
                  LBinop a o b -> flBinop (fold a) o (fold b)
                  LComp a o b -> flComp (fold a) o (fold b)
                  LQuant o v e -> flQuant o v (fold e)
                  LArray n e -> flArray n e
                  LNil -> flNil
                  NConst n -> fnConst n
                  NUnop o e -> fnUnop o (fold e)
                  NBinop a o b -> fnBinop (fold a) o (fold b)
                  NIf c a b -> fnIf (fold c) (fold a) (fold b)
                  NLen v -> fnLen v