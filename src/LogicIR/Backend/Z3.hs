module LogicIR.Backend.Z3 (lExprToZ3Ast) where

import Z3.Monad

import LogicIR.Expr
import LogicIR.Fold

lExprToZ3Ast :: LExpr -> Z3 AST
lExprToZ3Ast = foldLExpr lExprToZ3AstAlgebra

lExprToZ3AstAlgebra :: LExprAlgebra (Z3 AST)
lExprToZ3AstAlgebra = (flConst, flVar, flNot, flBinop, flComp, flQuant, flArray, fnConst, fnVar, fnUnop, fnBinop, fnArray) where
    flConst = undefined
    flVar = undefined
    flNot = undefined
    flBinop = undefined
    flComp = undefined
    flQuant = undefined
    flArray = undefined
    fnConst = undefined
    fnVar = undefined
    fnUnop = undefined
    fnBinop = undefined
    fnArray = undefined