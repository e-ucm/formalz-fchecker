module LogicIR.Frontend.Java (javaExpToLExpr) where

import Javawlp.Engine.Folds
import Javawlp.Engine.HelperFunctions

import Language.Java.Syntax
import Language.Java.Syntax.Types
import Language.Java.Parser
import Language.Java.Pretty

import LogicIR.Expr
import Data.Typeable

javaExpToLExpr :: Exp -> TypeEnv -> [TypeDecl] -> LExpr
javaExpToLExpr = foldExp javaExpToLExprAlgebra

nameToVar :: Name -> TypeEnv -> [TypeDecl] -> Var
nameToVar name env decls = let (arrayType, symbol) = (lookupType decls env name, prettyPrint name) in
    case arrayType of
        PrimType BooleanT -> Var (TPrim PBool) symbol
        PrimType IntT -> Var (TPrim PInt) symbol
        RefType (ArrayType (PrimType IntT)) -> Var (TArray (TPrim PInt)) symbol
        _ -> error $ "Unimplemented nameToVar for " ++ show (name, arrayType)

javaExpToLExprAlgebra :: ExpAlgebra (TypeEnv -> [TypeDecl] -> LExpr)
javaExpToLExprAlgebra = (fLit, fClassLit, fThis, fThisClass, fInstanceCreation, fQualInstanceCreation, fArrayCreate, fArrayCreateInit, fFieldAccess, fMethodInv, fArrayAccess, fExpName, fPostIncrement, fPostDecrement, fPreIncrement, fPreDecrement, fPrePlus, fPreMinus, fPreBitCompl, fPreNot, fCast, fBinOp, fInstanceOf, fCond, fAssign, fLambda, fMethodRef) where
    fLit lit _ _ = case lit of -- TODO: support more type literals?
                    Boolean b -> LConst b
                    Int n -> NConst (fromIntegral n)
                    Null -> LNil
                    _ -> error $ show $ lit
    fClassLit = error "fClassLit not supported..."
    fThis = error "fThis not supported..."
    fThisClass = error "fThisClass not supported..."
    fInstanceCreation = error "fInstanceCreation not supported..."
    fQualInstanceCreation = error "fQualInstanceCreation not supported..."
    fArrayCreate = undefined
    fArrayCreateInit = undefined
    fFieldAccess = undefined
    fMethodInv inv env decls = case inv of -- TODO: very hardcoded EDSL + lambdas cannot be { return expr; }
                                    MethodCall (Name [Ident "forall"]) [ExpName name, Lambda (LambdaSingleParam (Ident bound)) (LambdaExpression expr)]
                                        -> let (i, arr) = (Var (TPrim PInt) bound, nameToVar name env decls) in
                                            LQuant QAll i (LBinop (LBinop (LComp (LVar i) CGeq (NConst 0)) LAnd (LComp (LVar i) CLess (NLen arr))) LImpl (foldExp javaExpToLExprAlgebra expr env decls))
                                    MethodCall (Name [Ident "exists"]) [ExpName name, Lambda (LambdaSingleParam (Ident bound)) (LambdaExpression expr)]
                                        -> let (i, arr) = (Var (TPrim PInt) bound, nameToVar name env decls) in
                                            LQuant QAny i (LBinop (LBinop (LComp (LVar i) CGeq (NConst 0)) LAnd (LComp (LVar i) CLess (NLen arr))) LAnd (foldExp javaExpToLExprAlgebra expr env decls))
                                    _
                                        -> error $ "Unimplemented fMethodInv: " ++ show inv
    fArrayAccess arrayIndex env decls = case arrayIndex of -- TODO: type checking
                                             ArrayIndex (ExpName name) [ExpName index]
                                                -> LArray (nameToVar name env decls) [LVar (nameToVar index env decls)]
                                             _
                                                -> error $ "Multidimensional arrays are not supported: " ++ show (arrayIndex)
    fExpName name env decls = case name of -- TODO: type checking
                                   Name [Ident a, Ident "length"] -> NLen $ nameToVar (Name [Ident a]) env decls
                                   _ -> LVar $ nameToVar name env decls
    fPostIncrement = error "fPostIncrement has side effects..."
    fPostDecrement = error "fPostDecrement has side effects..."
    fPreIncrement = error "fPreIncrement has side effects..."
    fPreDecrement = error "fPreDecrement has side effects..."
    fPrePlus e env decls = e env decls
    fPreMinus e env decls = NUnop NNeg (e env decls)
    fPreBitCompl e env decls = NUnop NNot (e env decls)
    fPreNot e env decls = LNot (e env decls)
    fCast = undefined -- TODO: perhaps support cast for some types?
    fBinOp e1 op e2 env decls = let (a, b) = (e1 env decls, e2 env decls) in -- TODO: type checking?
                                case op of
                                     -- Integer
                                     Mult -> NBinop a NMul b
                                     Div -> NBinop a NDiv b
                                     Rem -> NBinop a NRem b
                                     Add -> NBinop a NAdd b
                                     Sub  -> NBinop a NSub b
                                     LShift -> NBinop a NShl b
                                     RShift -> NBinop a NShr b
                                     RRShift -> undefined
                                     And -> NBinop a NAnd b
                                     Or -> NBinop a NOr b
                                     Xor -> NBinop a NXor b
                                     -- Logical
                                     CAnd -> LBinop a LAnd b
                                     COr -> LBinop a LOr b
                                     -- Comparisons
                                     LThan -> LComp a CLess b
                                     GThan -> LComp a CGreater b
                                     LThanE -> LComp a CLeq b
                                     GThanE -> LComp a CGeq b
                                     Equal -> LComp a CEqual b
                                     NotEq -> LComp a CNEqual b
    fInstanceOf = undefined
    fCond c a b env decls = NIf (c env decls) (a env decls) (b env decls)
    fAssign = error "fAssign has side effects..."
    fLambda = error "fLambda should be handled by fMethodInv..."
    fMethodRef = undefined