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
foldNExpr (const, var, unop, binop, array) = fold where
    fold e = case e of
                  NConst n -> const n
                  NVar n -> var n
                  NUnop o e -> unop o (fold e)
                  NBinop a o b -> binop (fold a) o (fold b)
                  NArray n e -> array n (fold e)

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
foldLExpr (const, var, not, binop, comp, quant, array) = fold where
    fold e = case e of
                  LConst c -> const c
                  LVar n -> var n
                  LNot e -> not (fold e)
                  LBinop a o b -> binop (fold a) o (fold b)
                  LComp a o b -> comp a o b
                  LQuant o vs e -> quant o vs (fold e)
                  LArray n e -> array n e