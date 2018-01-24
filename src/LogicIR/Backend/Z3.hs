{-# LANGUAGE OverloadedStrings #-}
module LogicIR.Backend.Z3 (lExprToZ3Ast) where

import Z3.Monad
import Control.Monad.Trans (liftIO)

import LogicIR.Expr
import LogicIR.Fold
import LogicIR.Parser

lExprToZ3Ast :: LExpr -> Z3 AST
lExprToZ3Ast = foldLExpr lExprToZ3AstAlgebra

getSorts :: AST -> AST -> Z3 (String, String)
getSorts a b = do
  as <- getSort a
  bs <- getSort b
  return (show as, show bs)

-- TODO: support more types
lExprToZ3AstAlgebra :: LExprAlgebra (Z3 AST)
lExprToZ3AstAlgebra = (fConst, fVar, fUnop, fBinop, fIf, fQuant, fArray, fIsnull, fLen) where
    fConst c = case c of
      CBool b -> mkBool b
      CInt n -> mkInteger $toInteger n
      CReal f -> mkRealNum f
      CNil -> error "null constants cannot be used directly with Z3 (use LogicIR.Backend.Null)"
    fVar (Var t n) = do
      symbol <- mkStringSymbol n
      case t of
        "int" -> mkIntVar symbol
        "bool" -> mkBoolVar symbol
        "real" -> mkRealVar symbol
        "[int]" -> do
          intSort <- mkIntSort
          arraySort <- mkArraySort intSort intSort
          mkVar symbol arraySort
        "[bool]" -> do
          intSort <- mkIntSort
          arraySort <- mkBoolSort >>= mkArraySort intSort
          mkVar symbol arraySort
        _ -> error $ "unsupported type: " ++ show n
    fUnop o a' = do
      a <- a'
      case o of
        NNeg -> mkUnaryMinus a
        NNot -> mkBvnot a --
        LNot -> mkNot a
    fBinop a' o b' = do
      a <- a'
      b <- b'
      case o of
        NAdd     -> mkAdd [a, b]
        NSub     -> mkSub [a, b]
        NMul     -> mkMul [a, b]
        NDiv     -> mkDiv a b -- NOTE: signed division
        NRem     -> mkRem a b -- TODO: check if the correct remainder is taken
        NShl     -> mkBvshl a b --
        NShr     -> mkBvashr a b -- NOTE: signed shift right will keep the sign
        NAnd     -> mkBvand a b --
        NOr      -> mkBvor a b --
        NXor     -> mkBvxor a b --
        LAnd     -> mkAnd [a, b]
        LOr      -> mkOr [a, b]
        LImpl    -> mkImplies a b
        CEqual   -> mkEq a b
        CLess    -> mkLt a b
        CGreater -> mkGt a b
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
    fLen (Var (TArray _) n) = mkStringSymbol (n ++ "?length") >>= mkIntVar -- TODO: support proper array lengths
