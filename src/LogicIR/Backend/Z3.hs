module LogicIR.Backend.Z3 (lExprToZ3Ast) where

import Z3.Monad

import LogicIR.Expr
import LogicIR.Fold

lExprToZ3Ast :: LExpr -> Z3 AST
lExprToZ3Ast = foldLExpr lExprToZ3AstAlgebra

lExprToZ3AstAlgebra :: LExprAlgebra (Z3 AST)
lExprToZ3AstAlgebra = (flConst, flVar, flNot, flBinop, flComp, flQuant, flArray, flNil, fnConst, fnUnop, fnBinop, fnIf, fnLen) where
    flConst = undefined
    flVar = undefined
    flNot = undefined
    flBinop = undefined
    flComp = undefined
    flQuant = undefined
    flArray = undefined
    flNil = undefined
    fnConst = undefined
    fnUnop = undefined
    fnBinop = undefined
    fnIf = undefined
    fnLen = undefined