module LogicIR.Fold
       ( LAlgebra (..)
       , defLAlgebra
       , foldLExpr
       ) where

import LogicIR.Expr

-- | Fold algebra for logical expressions.
data LAlgebra r = LAlgebra
  { cns :: LConst -> r
  , var :: Var -> r
  , uni :: LUnop -> r -> r
  , bin :: r -> LBinop -> r -> r
  , iff :: r -> r -> r -> r
  , qnt :: QOp -> Var -> r -> r -> r
  , arr :: r -> r -> r
  , nll :: r -> r
  , len :: r -> r
  }

defLAlgebra :: LAlgebra LExpr
defLAlgebra = LAlgebra LConst LVar LUnop LBinop LIf LQuant LArray LIsnull LLen

-- | Fold for logical expressions.
foldLExpr :: LAlgebra r -> LExpr -> r
foldLExpr (LAlgebra fcns fvar funi fbin fiff fqnt farr fnll flen) = fold
  where fold e = case e of
                  LConst c       -> fcns c
                  LVar x         -> fvar x
                  LUnop o a      -> funi o (fold a)
                  LBinop x o y   -> fbin (fold x) o (fold y)
                  LIf c x y      -> fiff (fold c) (fold x) (fold y)
                  LQuant o y d a -> fqnt o y (fold d) (fold a)
                  LArray x a     -> farr (fold x) (fold a)
                  LIsnull x      -> fnll (fold x)
                  LLen x         -> flen (fold x)
