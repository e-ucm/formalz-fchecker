{-# LANGUAGE OverloadedStrings #-}
module LogicIR.TypeChecker (typeCheck) where

import Control.Monad.Except
import Control.Monad (unless)

import LogicIR.Expr
import LogicIR.Parser ()

-- | Type-check given expression.
typeCheck :: LExpr -> Either String LExpr
typeCheck lexp = typeOf lexp >> return lexp

-- | Get the type of an expression or a type error.
typeOf :: LExpr -> Either String Type
typeOf lexp = case lexp of
  LVar (Var t _) -> return t
  LConst c       -> case c of
    CBool _ -> return "bool"
    CInt _  -> return "int"
    CReal _ -> return "real"
    CNil    -> throwError "Type-checking before preprocessing null checks"
  LArray e e' -> do
    te <- typeOf e
    case te of
      TArray t' -> do
        te' <- typeOf e'
        te' == "int" ?? "(indexing): non-integer index"
        return t'
      _ -> throwError "(indexing): non-array variable"
  LIsnull e -> do
    t <- typeOf e
    isArray t ??  "(isNull): non-array argument"
    return "bool"
  LLen e -> do
    t <- typeOf e
    isArray t ??  "(length): non-array argument"
    return "int"
  LUnop o e -> do
    t <- typeOf e
    case o of
      NNeg -> do
        isNum t ?? "(-): non-numeric argument"
        return t
      LNot -> do
        (t == "bool") ?? "(!): non-boolean argument"
        return t
  LBinop e o e' -> do
    t <- typeOf e
    t' <- typeOf e'
    case o of
      NAdd -> do
        isNum t && isNum t' ?? "(+): non-numeric arguments"
        return $ coerce t t'
      NSub -> do
        isNum t && isNum t' ?? "(-): non-numeric arguments"
        return $ coerce t t'
      NMul -> do
        isNum t && isNum t' ?? "(*): non-numeric arguments"
        return $ coerce t t'
      NDiv -> do
        isNum t && isNum t' ?? "(/): non-numeric arguments"
        return $ coerce t t'
      NRem -> do
        isNum t && isNum t' ?? "(%): non-numeric arguments"
        return $ coerce t t'
      CEqual -> do
        isNum t && isNum t' ?? "(==): non-numeric arguments"
        return "bool"
      CLess -> do
        isNum t && isNum t' ?? "(<): non-numeric arguments"
        return "bool"
      CGreater -> do
        isNum t && isNum t' ?? "(>): non-numeric arguments"
        return "bool"
      LAnd -> do
        t == "bool" && t' == "bool" ?? "(&&): non-boolean arguments"
        return "bool"
      LOr -> do
        t == "bool" && t' == "bool" ?? "(||): non-boolean arguments"
        return "bool"
      LImpl -> do
        t == "bool" && t' == "bool" ?? "(==>): non-boolean arguments"
        return "bool"
      LEqual -> do
        t == "bool" && t' == "bool" ?? "(<==>): non-boolean arguments"
        return "bool"
  LIf c e e'     -> do
    ct <- typeOf c
    ct == "bool" ?? "(if): non-boolean guard"
    t <- typeOf e
    t' <- typeOf e'
    t == t' ?? "(if): branches of different type"
    return t
  LQuant _ _ d e -> do
    dt <- typeOf d
    t <- typeOf e
    dt == "bool" ?? "(quantifier): non-boolean domain [" ++ show dt ++ "]"
    t == "bool" ?? "(quantifier): non-boolean body"
    return "bool"
  where
    infix 2 ??
    (??) :: Bool -> String -> Either String ()
    predicate ?? err = unless predicate $ throwError err
    isNum :: Type -> Bool
    isNum t = t `elem` ["int", "real"]
    isArray :: Type -> Bool
    isArray (TArray _) = True
    isArray _          = False
    coerce :: Type -> Type -> Type
    coerce t t' = if "real" `elem` [t, t'] then "real" else "int"
