{-# LANGUAGE OverloadedStrings #-}
module LogicIR.Backend.Z3.Z3 where

import Control.Monad.Trans (liftIO)
import qualified Data.Map as M
import Z3.Monad

import LogicIR.Expr
import LogicIR.Fold
import LogicIR.Parser

lExprToZ3Ast :: LExpr -> Z3 AST
lExprToZ3Ast = foldLExpr lExprToZ3AstAlgebra

lExprToZ3AstAlgebra :: LExprAlgebra (Z3 AST)
lExprToZ3AstAlgebra =
  (fConst, genVar, fUnop, fBinop, fIf, fQuant, fArray, fIsnull, fLen) where
  fConst c = case c of
    CBool b -> mkBool b
    CInt n -> mkInteger $ toInteger n
    CReal f -> mkRealNum f
    CNil -> error "null constants cannot be used directly with Z3 (use LogicIR.Backend.Null)"
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
        vApp <- genVar v >>= toApp
        case o of
          QAll -> do e <- mkImplies d a
                     mkForallConst [] [vApp] e
          QAny -> do e <- mkAnd [d, a]
                     mkExistsConst [] [vApp] e
      _ -> error $ "unsupported quantifier domain type: " ++ show (o, v)
  fArray v a' = do
    v <- genVar v
    a <- a'
    mkSelect v a
  fIsnull (Var (TArray _) n) = genNullVar n
  fLen (Var (TArray _) n) = genLenVar n -- TODO: support proper array lengths

-- | Get formula's free variables (to be included in model).
type FreeVars = M.Map String AST
freeVars2 :: LExpr -> LExpr -> Z3 FreeVars
freeVars2 l l' = M.unions <$> mapM (foldLExpr freeVarsAlgebra) [l, l']
freeVars :: LExpr -> Z3 FreeVars
freeVars = foldLExpr freeVarsAlgebra
freeVarsAlgebra :: LExprAlgebra (Z3 FreeVars)
freeVarsAlgebra =
  (fConst, fVar, fUnop, fBinop, fIf, fQuant, fArray, fIsnull, fLen) where
  fConst _ = return M.empty
  fVar v@(Var t n) = M.singleton n <$> genVar v
  fUnop _ r = r
  fBinop r _ r' = M.unions <$> sequence [r, r']
  fIf r r' r'' = M.unions <$> sequence [r, r', r'']
  fQuant _ (Var _ n) d r = M.unions . fmap (M.delete n) <$> sequence [r, d]
  fArray v _ = M.unions <$> sequence [fIsnull v, fLen v, fVar v]
  fIsnull (Var (TArray _) n) = M.singleton (n ++ "?null") <$> genNullVar n
  fLen (Var (TArray _) n) = M.singleton (n ++ "?length") <$> genLenVar n

-- | Generate a new Z3 variable, depending on the expression's type.
genVar :: Var -> Z3 AST
genVar (Var t n) =
  mkStringSymbol n >>=
    case t of
      "int"    -> mkIntVar
      "bool"   -> mkBoolVar
      "real"   -> mkRealVar
      "[int]"  -> mkIntArrayVar
      "[real]" -> mkRealArrayVar
      "[bool]" -> mkBoolArrayVar
      _        -> error $ "unsupported type: " ++ show t
  where mkArrayVar mkValSort symbol = do
          intSort <- mkIntSort
          valSort <- mkValSort
          arraySort <- mkArraySort intSort valSort
          mkVar symbol arraySort
        mkRealArrayVar = mkArrayVar mkRealSort
        mkIntArrayVar = mkArrayVar mkIntSort
        mkBoolArrayVar = mkArrayVar mkBoolSort
genNullVar :: String -> Z3 AST
genNullVar s = genVar $ Var "bool" (s ++ "?null")
genLenVar :: String -> Z3 AST
genLenVar s = genVar $ Var "int" (s ++ "?length")
