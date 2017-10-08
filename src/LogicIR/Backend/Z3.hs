module LogicIR.Backend.Z3 (lExprToZ3Ast) where

import Z3.Monad

import LogicIR.Expr
import LogicIR.Fold

lExprToZ3Ast :: LExpr -> Z3 AST
lExprToZ3Ast = foldLExpr lExprToZ3AstAlgebra

lExprToZ3AstAlgebra :: LExprAlgebra (Z3 AST)
lExprToZ3AstAlgebra = (flConst, flVar, flNot, flBinop, flComp, flQuant, flArray, flNil, fnConst, fnUnop, fnBinop, fnIf, fnLen) where
    flConst b = mkBool b
    flVar (Var t n) = do symbol <- mkStringSymbol n
                         case t of
                              TPrim PInt -> mkIntVar symbol
                              TPrim PBool -> mkBoolVar symbol
                              _ -> error $ show n
    flNot a = a >>= mkNot
    flBinop a' o b' = do a <- a'
                         b <- b'
                         case o of
                            LAnd -> mkAnd [a, b]
                            LOr -> mkOr [a, b]
                            LImpl -> mkImplies a b
                            LBicond -> undefined
    flComp a' o b' = do a <- a'
                        b <- b'
                        case o of
                            CEqual -> mkEq a b
                            CNEqual -> mkEq a b >>= mkNot
                            CLess -> mkLt a b
                            CGreater -> mkGt a b
                            CLeq -> mkLe a b
                            CGeq -> mkGe a b
    flQuant = undefined
    flArray = undefined
    flNil = undefined
    fnConst n = mkInteger (fromIntegral n)
    fnUnop = undefined
    fnBinop a' o b' = do a <- a'
                         b <- b'
                         case o of
                            NAdd -> mkAdd [a, b]
                            _ -> undefined
    fnIf = undefined
    fnLen = undefined