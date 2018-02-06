module LogicIR.Fold where

import LogicIR.Expr

-- | Fold algebra for logical expressions.
type LExprAlgebra r = (LConst -> r, -- LConst
                       Var -> r, -- LVar
                       LUnop -> r -> r, -- LUnop
                       r -> LBinop -> r -> r, -- LBinop
                       r -> r -> r -> r, -- LIf
                       QOp -> Var -> r -> r -> r, -- LQuant
                       Var -> r -> r, -- LArray
                       Var -> r, -- LIsnull
                       Var -> r -- LLen
                      )

-- | Fold for logical expressions.
foldLExpr :: LExprAlgebra r -> LExpr -> r
foldLExpr (fConst, fVar, fUnop, fBinop, fIf, fQuant, fArray, fIsnull, fLen) = fold where
    fold e = case e of
                  LConst c       -> fConst c
                  LVar v         -> fVar v
                  LUnop o a      -> fUnop o (fold a)
                  LBinop a o b   -> fBinop (fold a) o (fold b)
                  LIf c a b      -> fIf (fold c) (fold a) (fold b)
                  LQuant o b d a -> fQuant o b (fold d) (fold a)
                  LArray v a     -> fArray v (fold a)
                  LIsnull v      -> fIsnull v
                  LLen v         -> fLen v
