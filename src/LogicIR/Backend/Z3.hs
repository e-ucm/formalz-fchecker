module LogicIR.Backend.Z3 (lExprToZ3Ast) where

import Z3.Monad

import LogicIR.Expr
import LogicIR.Fold

lExprToZ3Ast :: LExpr -> Z3 AST
lExprToZ3Ast = foldLExpr lExprToZ3AstAlgebra

-- TODO: support more types
lExprToZ3AstAlgebra :: LExprAlgebra (Z3 AST)
lExprToZ3AstAlgebra = (flConst, flVar, flNot, flBinop, flComp, flQuant, flArray, flNil, fnConst, fnUnop, fnBinop, fnIf, fnLen) where
    flConst b = mkBool b
    flVar (Var t n) = do symbol <- mkStringSymbol n
                         case t of
                              TPrim PInt32 -> mkBvVar symbol 32
                              TPrim PBool -> mkBoolVar symbol
                              TArray (TPrim PInt32) -> do intSort <- mkBvSort 32
                                                          arraySort <- mkArraySort intSort intSort
                                                          mkVar symbol arraySort
                              TArray (TPrim PBool) -> do intSort <- mkBvSort 32
                                                         arraySort <- mkBoolSort >>= mkArraySort intSort
                                                         mkVar symbol arraySort
                              _ -> error $ "unsupported type: " ++ show n
    flNot a = a >>= mkNot
    flBinop a' o b' = do a <- a'
                         b <- b'
                         case o of
                            LAnd -> mkAnd [a, b]
                            LOr -> mkOr [a, b]
                            LImpl -> mkImplies a b
    flComp a' o b' = do a <- a'
                        b <- b'
                        case o of
                            CEqual -> mkEq a b
                            CNEqual -> mkEq a b >>= mkNot
                            CLess -> mkBvslt a b
                            CGreater -> mkBvsgt a b
                            CLeq -> mkBvsle a b
                            CGeq -> mkBvsge a b
    flQuant o v@(Var t n) a' = do a <- a'
                                  case t of
                                       TPrim PInt32 -> do vApp <- flVar v >>= toApp
                                                          case o of
                                                               QAll -> mkForallConst [] [vApp] a
                                                               QAny -> mkExistsConst [] [vApp] a
                                       _ -> error $ "unsupported quantifier domain type: " ++ show (o, v)
    flArray v a' = do v <- flVar v
                      a <- a'
                      mkSelect v a
    flNil = do intSort <- mkBvSort 32
               zero <- mkBitvector 32 0 -- (isNull, data, length) TODO: support proper null types
               mkConstArray intSort zero
    fnConst n = mkBitvector 32 (fromIntegral n)
    fnUnop o a' = do a <- a'
                     case o of
                          NNeg -> mkBvneg a
                          NNot -> mkBvnot a
    fnBinop a' o b' = do a <- a'
                         b <- b'
                         case o of
                            NAdd -> mkBvadd a b
                            NSub -> mkBvsub a b
                            NMul -> mkBvmul a b
                            NDiv -> mkBvsdiv a b -- NOTE: signed division
                            NRem -> mkBvsrem a b -- TODO: check if the correct remainder is taken
                            NShl -> mkBvshl a b
                            NShr -> mkBvashr a b -- NOTE: signed shift right will keep the sign
                            NAnd -> mkBvand a b
                            NOr -> mkBvor a b
                            NXor -> mkBvxor a b
    fnIf c' a' b' = do c <- c'
                       a <- a'
                       b <- b'
                       mkIte c a b
    fnLen (Var (TArray (TPrim _)) n) = mkStringSymbol (n ++ ".length") >>= flip mkBvVar 32 -- TODO: support proper array lengths
