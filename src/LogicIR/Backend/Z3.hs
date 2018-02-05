{-# LANGUAGE OverloadedStrings #-}
module LogicIR.Backend.Z3 where

import Z3.Monad
import Control.Monad.Trans (liftIO)

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
      CInt n -> mkInteger $toInteger n
      CReal f -> mkRealNum f
      CNil -> error "null constants cannot be used directly with Z3 (use LogicIR.Backend.Null)"
    fVar (Var t n) =
      mkStringSymbol n >>=
        case t of
          "int" -> mkIntVar
          "bool" -> mkBoolVar
          "real" -> mkRealVar
          "[int]" -> mkIntArrayVar
          "[real]" -> mkRealArrayVar
          "[bool]" -> mkBoolArrayVar
          _ -> error $ "unsupported type: " ++ show t
    fUnop o a' = do
      a <- a'
      case o of
        NNeg -> mkUnaryMinus a
        LNot -> mkNot a
    fBinop a' o b' = do
      a <- a'
      b <- b'
      case o of
        NAdd     -> mkAdd [a, b]
        NSub     -> mkSub [a, b]
        NMul     -> mkMul [a, b]
        NDiv     -> mkDiv a b
        NRem     -> mkRem a b
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

mkArrayVar :: Z3 Sort -> Symbol -> Z3 AST
mkArrayVar mkValSort symbol = do
  intSort <- mkIntSort
  valSort <- mkValSort
  arraySort <- mkArraySort intSort valSort
  mkVar symbol arraySort
mkRealArrayVar = mkArrayVar mkRealSort
mkIntArrayVar = mkArrayVar mkIntSort
mkBoolArrayVar = mkArrayVar mkBoolSort

getSorts :: AST -> AST -> Z3 (String, String)
getSorts a b = do
  as <- getSort a
  bs <- getSort b
  return (show as, show bs)
