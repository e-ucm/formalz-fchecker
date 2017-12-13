{-# LANGUAGE OverloadedStrings #-}
module LogicIR.Backend.Z3 (lExprToZ3Ast) where

import Z3.Monad

import LogicIR.Expr
import LogicIR.Fold
import LogicIR.Parser

lExprToZ3Ast :: LExpr -> Z3 AST
lExprToZ3Ast = foldLExpr lExprToZ3AstAlgebra

-- TODO: support more types
lExprToZ3AstAlgebra :: LExprAlgebra (Z3 AST)
lExprToZ3AstAlgebra = (fConst, fVar, fUnop, fBinop, fIf, fQuant, fArray, fIsnull, fLen) where
    fConst c = case c of
      CBool b -> mkBool b
      CInt n -> mkBitvector 32 (fromIntegral n)
      CNil -> error "null constants cannot be used directly with Z3 (use LogicIR.Backend.Null)"
    fVar (Var t n) = do
      symbol <- mkStringSymbol n
      case t of
        "int" -> mkBvVar symbol 32
        "bool" -> mkBoolVar symbol
        "[int]" -> do
          intSort <- mkBvSort 32
          arraySort <- mkArraySort intSort intSort
          mkVar symbol arraySort
        "[bool]" -> do
          intSort <- mkBvSort 32
          arraySort <- mkBoolSort >>= mkArraySort intSort
          mkVar symbol arraySort
        _ -> error $ "unsupported type: " ++ show n
    fUnop o a' = do
      a <- a'
      case o of
        NNeg -> mkBvneg a
        NNot -> mkBvnot a
        LNot -> mkNot a
    fBinop a' o b' = do
      a <- a'
      b <- b'
      case o of
        NAdd     -> mkBvadd a b
        NSub     -> mkBvsub a b
        NMul     -> mkBvmul a b
        NDiv     -> mkBvsdiv a b -- NOTE: signed division
        NRem     -> mkBvsrem a b -- TODO: check if the correct remainder is taken
        NShl     -> mkBvshl a b
        NShr     -> mkBvashr a b -- NOTE: signed shift right will keep the sign
        NAnd     -> mkBvand a b
        NOr      -> mkBvor a b
        NXor     -> mkBvxor a b
        LAnd     -> mkAnd [a, b]
        LOr      -> mkOr [a, b]
        LImpl    -> mkImplies a b
        CEqual   -> mkEq a b
        CLess    -> mkBvslt a b
        CGreater -> mkBvsgt a b
    fIf c' a' b' = do
      c <- c'
      a <- a'
      b <- b'
      mkIte c a b
    fQuant o v@(Var t n) d' a' = do
      a <- a'
      d <- d'
      case t of
        "int" -> do
          vApp <- fVar v >>= toApp
          case o of
            QAll -> do e <- mkImplies d a
                       mkForallConst [] [vApp] e
            QAny -> do e <- mkAnd [d, a]
                       mkExistsConst [] [vApp] e
        _ -> error $ "unsupported quantifier domain type: " ++ show (o, v)
    fArray v a' = do
      v <- fVar v
      a <- a'
      mkSelect v a
    fIsnull (Var (TArray _) n) = mkStringSymbol (n ++ "?null") >>= mkBoolVar
    fLen (Var (TArray _) n) = mkStringSymbol (n ++ "?length") >>= flip mkBvVar 32 -- TODO: support proper array lengths
