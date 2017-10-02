module LogicIR.Fold where

import LogicIR.Expr

-- Fold algebra for logical expressions
type LExprAlgebra r = (Bool -> r, -- LConst
                       Var -> r, -- LVar
                       r -> r, -- LNot
                       r -> LBinop -> r -> r, -- LBinop
                       r -> COp -> r -> r, -- LComp
                       QOp -> [Var] -> r -> r, -- LQuant
                       Var -> [NExpr] -> r, -- LArray
                       Int -> r, -- NConst
                       Var -> r, -- NVar
                       NUnop -> r -> r, -- NUnop
                       r -> NBinop -> r -> r, -- NBinop
                       Var -> [NExpr] -> r, -- NArray
                       r -> r -> r -> r -- NIf
                      )

-- Fold for logical expressions
foldLExpr :: LExprAlgebra r -> LExpr -> r
foldLExpr (flConst, flVar, flNot, flBinop, flComp, flQuant, flArray, fnConst, fnVar, fnUnop, fnBinop, fnArray, fnIf) = fold where
    fold e = case e of
                  LConst c -> flConst c
                  LVar n -> flVar n
                  LNot e -> flNot (fold e)
                  LBinop a o b -> flBinop (fold a) o (fold b)
                  LComp a o b -> flComp (fold a) o (fold b)
                  LQuant o vs e -> flQuant o vs (fold e)
                  LArray n e -> flArray n e
                  NConst n -> fnConst n
                  NVar n -> fnVar n
                  NUnop o e -> fnUnop o (fold e)
                  NBinop a o b -> fnBinop (fold a) o (fold b)
                  NArray n e -> fnArray n e
                  NIf c a b -> fnIf (fold c) (fold a) (fold b)