module LogicIR.Backend.Z3 (lExprToZ3Ast) where

import Z3.Monad

import LogicIR.Expr
import LogicIR.Fold

nExprToZ3Ast :: NExpr -> Z3 AST
nExprToZ3Ast = foldNExpr nExprToZ3AstAlgebra

nExprToZ3AstAlgebra :: NExprAlgebra (Z3 AST)
nExprToZ3AstAlgebra = (fConst, fVar, fUnop, fBinop, fArray) where
    fConst = undefined
    fVar = undefined
    fUnop = undefined
    fBinop = undefined
    fArray = undefined

lExprToZ3Ast :: LExpr -> Z3 AST
lExprToZ3Ast = foldLExpr lExprToZ3AstAlgebra

lExprToZ3AstAlgebra :: LExprAlgebra (Z3 AST)
lExprToZ3AstAlgebra = (fConst, fVar, fNot, fBinop, fComp, fQuant, fArray) where
    fConst = undefined
    fVar = undefined
    fNot = undefined
    fBinop = undefined
    fComp = undefined
    fQuant = undefined
    fArray = undefined