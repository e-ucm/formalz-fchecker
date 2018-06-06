{-# LANGUAGE OverloadedStrings #-}
module LogicIR.Frontend.Java (javaExpToLExpr) where

import JavaHelpers.Folds
import JavaHelpers.HelperFunctions

import Language.Java.Pretty
import Language.Java.Syntax

import           Control.Monad.State
import qualified Data.Map            as M

import LogicIR.Expr
import LogicIR.Parser      ()
import LogicIR.TypeChecker (typeOf)

-- Convert a Java expression to a LIR expression.
-- The transformation is stateful, since we need to generate fresh variables
-- when introducing variables with the `with` construct.
javaExpToLExpr :: Exp -> TypeEnv -> [TypeDecl] -> LExpr
javaExpToLExpr e env d = evalState (foldExp javaExpToLExprAlgebra e env d) 0

javaExpToLExprAlgebra :: ExpAlgebra (TypeEnv -> [TypeDecl] -> State Int LExpr)
javaExpToLExprAlgebra =
  (fLit, fClassLit, fThis, fThisClass, fInstanceCreation, fQualInstanceCreation,
   fArrayCreate, fArrayCreateInit, fFieldAccess, fMethodInv, fArrayAccess, fExpName,
   fPostIncrement, fPostDecrement, fPreIncrement, fPreDecrement, fPrePlus, fPreMinus,
   fPreBitCompl, fPreNot, fCast, fBinOp, fInstanceOf, fCond, fAssign, fLambda, fMethodRef)
  where
    fLit lit _ _ = return $
      case lit of
        Boolean t -> b t
        Int i     -> n $ fromInteger i
        Float i   -> r i
        Double i  -> r i
        Null      -> "null"
        _         -> error $ "Unsupported type: " ++ show lit
    fPrePlus e = e
    fPreMinus e env decls = LUnop NNeg <$> e env decls
    fPreNot e env decls = LUnop LNot <$> e env decls
    fCond c a b_ env d = LIf <$> c env d <*> a env d <*> b_ env d
    fBinOp e1 op e2 env decls =
      bop <$> e1 env decls <*> e2 env decls
      where
        bop =
          case op of
            -- Integer
            Mult    -> (.*)
            Div     -> (./)
            Rem     -> (.%)
            Add     -> (.+)
            Sub     -> (.-)
            RRShift -> undefined
            -- Logical
            CAnd    -> (.&&)
            COr     -> (.||)
            -- Comparisons
            LThan   -> (.<)
            GThan   -> (.>)
            LThanE  -> (.<=)
            GThanE  -> (.>=)
            Equal   -> (.==)
            NotEq   -> (.!=)
            _       -> error $ "Unsupported operation: " ++ prettyPrint op
    fExpName name env decls = return $
      case name of
        Name [Ident a, Ident "length"] ->
          LLen $ nameToVar (Name [Ident a]) env decls
        _ ->
          LVar $ nameToVar name env decls
    fArrayAccess arrayIndex env decls =
      case arrayIndex of
        ArrayIndex (ExpName name) [expr] ->
          LArray (nameToVar name env decls) <$>
            foldExp javaExpToLExprAlgebra expr env decls
        _ -> error $ "Multidimensional arrays are not supported: " ++ prettyPrint arrayIndex
    fMethodInv inv env decls =
      case inv of -- NOTE: EDSL only support single-param expression lambdas
        -- Java: imp(exp1, exp2);
        MethodCall (Name [Ident "imp"]) [exp1, exp2] ->
          (.==>) <$> refold exp1 <*> refold exp2
        -- Java: with(exp1, bound -> exp2);
        -- NOTE: Need to introduce a fresh variable here
        MethodCall (Name [Ident "with"]) [exp1, Lambda (LambdaSingleParam x) (LambdaExpression exp2)] -> do
          x' <- (("TEMP_" ++) . show) <$> get
          modify (+ 1)
          let sMap = M.singleton x (Ident x')
          e1 <- refold $ substExp exp1 sMap
          let (Right typ) = typeOf e1
          let env' = env ++ [(Name [Ident x'], typeToType' typ)]
          e2 <- foldExp javaExpToLExprAlgebra (substExp exp2 sMap) env' decls
          return $ (LVar (var x' typ) .== e1) .==> e2
        -- Java: method(name, bound -> expr);
        MethodCall (Name [Ident method]) [ExpName name, Lambda (LambdaSingleParam (Ident bound)) (LambdaExpression expr)]
            -> quant method name bound expr
        -- Java: method(name, rbegin, rend, bound -> expr);
        MethodCall (Name [Ident method]) [ExpName name, rbegin, rend, Lambda (LambdaSingleParam (Ident bound)) (LambdaExpression expr)]
            -> quantr method name rbegin rend bound expr
        _ -> error $ "Unimplemented fMethodInv: " ++ prettyPrint inv
        where quant method name bound expr =
                let i = Var (TPrim PInt) bound
                    (zero, len) = (LConst (CInt 0), LLen (nameToVar name env decls))
                in case method of
                          "forall" -> lquantr QAll i zero len expr
                          "exists" -> lquantr QAny i zero len expr
                          _ -> error $ "Unimplemented fMethodInv: " ++ prettyPrint inv
              quantr method name rbegin rend bound expr = do
                begin <- refold rbegin
                end <- refold rend
                let (i, _) = (Var (TPrim PInt) bound, nameToVar name env decls)
                case method of
                  "forallr" -> lquantr QAll i begin end expr
                  "existsr" -> lquantr QAny i begin end expr
                  _ -> error $ "Unimplemented fMethodInv: " ++ prettyPrint inv
              lquantr op i begin end expr =
                LQuant op i (LBinop (v i .>= begin) LAnd (LBinop (LVar i) CLess end)) <$> refold expr
              refold expr =
                foldExp javaExpToLExprAlgebra expr env decls
    fPreBitCompl _ _ _ = no "PreBitCompl"; fFieldAccess = no "FieldAccess"
    fInstanceOf = no "InstanceOf"; fAssign = no "Assign"; fLambda = no "Lambda"
    fMethodRef = no "MethodRef"; fPostIncrement = no "PostIncrement"
    fPostDecrement = no "PostDecrement"; fPreIncrement = no "PreIncrement"
    fPreDecrement = no "PreDecrement"; fClassLit = no "ClassLit"
    fThis = no "This"; fThisClass = no "ThisClass"
    fInstanceCreation = no "InstanceCreation"; fArrayCreate = no "ArrayCreate"
    fQualInstanceCreation = no "QualInstanceCreation"; fCast = no "Cast"
    fArrayCreateInit = no "ArrayCreateInit"
    no = error . ("[javaExpToLExp] " ++) . (++ " is not supported")

-- Substitute a variable for another over a Java expression.
type Substitution a = M.Map a a

substExp :: Exp -> Substitution Ident -> Exp
substExp e substMap =
  let sb ex = substExp ex substMap
  in case e of
    Lit l              -> Lit l
    PrePlus e'         -> PrePlus (sb e')
    PreMinus e'        -> PreMinus (sb e')
    PreNot e'          -> PreNot (sb e')
    BinOp e1 op e2     -> BinOp (sb e1) op (sb e2)
    Cond e1 e2 e3      -> Cond (sb e1) (sb e2) (sb e3)
    ExpName (Name xs) ->
      ExpName $ Name $ (\x -> M.findWithDefault x x substMap) <$> xs
    MethodInv (MethodCall f args) ->
      MethodInv $ MethodCall f (sb <$> args)
    ArrayAccess (ArrayIndex eArr eIs) ->
      ArrayAccess $ ArrayIndex (sb eArr) (sb <$> eIs)
    Lambda p@(LambdaSingleParam x) (LambdaExpression e') ->
      Lambda p (LambdaExpression $ substExp e' (M.delete x substMap))
    _ -> error "substExp: not supported"

-- Converts a name to a LogicIR.Var, it queries the type environment to find the correct type.
nameToVar :: Name -> TypeEnv -> [TypeDecl] -> Var
nameToVar name env decls =
  var (prettyPrint name) (typeToType $ lookupType decls env name)

-- Lookup the LogicIR type of a Java variable (in scope).
typeToType :: Language.Java.Syntax.Type -> LogicIR.Expr.Type
typeToType typ = case typ of
  PrimType c -> TPrim $
    case c of
      BooleanT -> PBool
      ShortT   -> PInt
      IntT     -> PInt
      LongT    -> PInt
      FloatT   -> PReal
      DoubleT  -> PReal
      _        -> error $ "Unsupported primitive type: " ++ prettyPrint c
  RefType (ArrayType t') -> TArray $ typeToType t'
  _ -> error $ "Unsupported type: " ++ prettyPrint typ

typeToType' :: LogicIR.Expr.Type -> Language.Java.Syntax.Type
typeToType' typ = case typ of
  TPrim c -> PrimType $
    case c of
      PBool -> BooleanT
      PInt  -> LongT
      PReal -> DoubleT
  TArray t' -> RefType $ ArrayType $ typeToType' t'
