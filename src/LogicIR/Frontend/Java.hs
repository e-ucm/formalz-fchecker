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

-- Converts a name to a LogicIR.Var, it queries the type environment to find the correct type.
nameToVar :: Name -> TypeEnv -> [TypeDecl] -> Var
nameToVar name env decls = let (arrayType, symbol) = (lookupType decls env name, prettyPrint name) in
    case arrayType of
        PrimType BooleanT -> Var (TPrim PBool) symbol
        PrimType IntT -> Var (TPrim PInt32) symbol
        RefType (ArrayType (PrimType IntT)) -> Var (TArray (TPrim PInt32)) symbol
        _ -> error $ "Unimplemented nameToVar for " ++ show (name, arrayType)

javaExpToLExprAlgebra :: ExpAlgebra (TypeEnv -> [TypeDecl] -> LExpr)
javaExpToLExprAlgebra = (fLit, fClassLit, fThis, fThisClass, fInstanceCreation, fQualInstanceCreation, fArrayCreate, fArrayCreateInit, fFieldAccess, fMethodInv, fArrayAccess, fExpName, fPostIncrement, fPostDecrement, fPreIncrement, fPreDecrement, fPrePlus, fPreMinus, fPreBitCompl, fPreNot, fCast, fBinOp, fInstanceOf, fCond, fAssign, fLambda, fMethodRef) where
    fLit lit _ _ = case lit of -- TODO: support more type literals?
                    Boolean b -> LConst (CBool b)
                    Int n -> LConst (CInt (fromIntegral n))
                    Null -> LConst CNil
                    _ -> error $ show lit
    fClassLit = error "fClassLit not supported..."
    fThis = error "fThis not supported..."
    fThisClass = error "fThisClass not supported..."
    fInstanceCreation = error "fInstanceCreation not supported..."
    fQualInstanceCreation = error "fQualInstanceCreation not supported..."
    fArrayCreate = error "fArrayCreate not supported..."
    fArrayCreateInit = error "fArrayCreateInit not supported..."
    fFieldAccess = undefined {-case fieldAccess of -- TODO: implement field accesses
                        PrimaryFieldAccess e id         -> case e of
                                                                InstanceCreation _ t args _ -> undefined
                                                                _ -> undefined
                        SuperFieldAccess id             -> mkStringSymbol (prettyPrint (Name [id])) >>= mkIntVar
                        ClassFieldAccess (Name name) id -> mkStringSymbol (prettyPrint (Name (name ++ [id]))) >>= mkIntVar -}
    fMethodInv inv env decls = case inv of -- TODO: very hardcoded EDSL + lambdas cannot be { return expr; } + ranged
                                    -- Java: method(name, bound -> expr);
                                    MethodCall (Name [Ident method]) [ExpName name, Lambda (LambdaSingleParam (Ident bound)) (LambdaExpression expr)]
                                        -> quant method name bound expr
                                    -- Java: method(name, bound -> { return expr; });
                                    MethodCall (Name [Ident method]) [ExpName name, Lambda (LambdaSingleParam (Ident bound)) (LambdaBlock (Block [BlockStmt (Return (Just expr))]))]
                                        -> quant method name bound expr
                                    -- Java: method(name, rbegin, rend, bound -> expr);
                                    MethodCall (Name [Ident method]) [ExpName name, rbegin, rend, Lambda (LambdaSingleParam (Ident bound)) (LambdaExpression expr)]
                                        -> quantr method name rbegin rend bound expr
                                    -- Java: method(name, rbegin, rend, bound -> { return expr; });
                                    MethodCall (Name [Ident method]) [ExpName name, rbegin, rend, Lambda (LambdaSingleParam (Ident bound)) (LambdaBlock (Block [BlockStmt (Return (Just expr))]))]
                                        -> quantr method name rbegin rend bound expr
                                    _
                                        -> error $ "Unimplemented fMethodInv: " ++ show inv
                                    where quant method name bound expr = let i = Var (TPrim PInt32) bound in
                                                                         let (zero, len) = (LConst (CInt 0), LLen (nameToVar name env decls)) in
                                                                         case method of
                                                                              "forall" -> lquantr QAll i zero len expr
                                                                              "exists" -> lquantr QAny i zero len expr
                                                                              _ -> error $ "Unimplemented fMethodInv: " ++ show inv
                                          quantr method name rbegin rend bound expr = let (begin, end) = (refold rbegin, refold rend) in
                                                                                      let (i, arr) = (Var (TPrim PInt32) bound, nameToVar name env decls) in
                                                                                      case method of
                                                                                           "forallr" -> lquantr QAll i begin end expr
                                                                                           "existsr" -> lquantr QAny i begin end expr
                                                                                           _ -> error $ "Unimplemented fMethodInv: " ++ show inv
                                          lquantr op i begin end expr = LQuant op i (LBinop (LBinop (LVar i) CGeq begin) LAnd (LBinop (LVar i) CLess end)) (refold expr)
                                          refold expr = foldExp javaExpToLExprAlgebra expr env decls
    fArrayAccess arrayIndex env decls = case arrayIndex of -- TODO: type checking
                                             ArrayIndex (ExpName name) [expr]
                                                -> LArray (nameToVar name env decls) (javaExpToLExpr expr env decls)
                                             _
                                                -> error $ "Multidimensional arrays are not supported: " ++ show arrayIndex
    fExpName name env decls = case name of -- TODO: type checking + check implicit `this.name`
                                   Name [Ident a, Ident "length"] -> LLen $ nameToVar (Name [Ident a]) env decls
                                   _ -> LVar $ nameToVar name env decls
    fPostIncrement = error "fPostIncrement has side effects..."
    fPostDecrement = error "fPostDecrement has side effects..."
    fPreIncrement = error "fPreIncrement has side effects..."
    fPreDecrement = error "fPreDecrement has side effects..."
    fPrePlus e = e
    fPreMinus e env decls = LUnop NNeg (e env decls)
    fPreBitCompl e env decls = LUnop NNot (e env decls)
    fPreNot e env decls = LUnop LNot (e env decls)
    fCast = error "fCast is not supported..." -- TODO: perhaps support cast for some types?
    fBinOp e1 op e2 env decls = let (a, b) = (e1 env decls, e2 env decls) in -- TODO: type checking?
                                case op of
                                     -- Integer
                                     Mult -> LBinop a NMul b
                                     Div -> LBinop a NDiv b
                                     Rem -> LBinop a NRem b
                                     Add -> LBinop a NAdd b
                                     Sub  -> LBinop a NSub b
                                     LShift -> LBinop a NShl b
                                     RShift -> LBinop a NShr b
                                     RRShift -> undefined
                                     And -> LBinop a NAnd b
                                     Or -> LBinop a NOr b
                                     Xor -> LBinop a NXor b
                                     -- Logical
                                     CAnd -> LBinop a LAnd b
                                     COr -> LBinop a LOr b
                                     -- Comparisons
                                     LThan -> LBinop a CLess b
                                     GThan -> LBinop a CGreater b
                                     LThanE -> LBinop a CLeq b
                                     GThanE -> LBinop a CGeq b
                                     Equal -> LBinop a CEqual b
                                     NotEq -> LBinop a CNEqual b
    fInstanceOf = error "fInstanceOf is not supported..."
    fCond c a b env decls = LIf (c env decls) (a env decls) (b env decls)
    fAssign = error "fAssign has side effects..."
    fLambda = error "fLambda should be handled by fMethodInv..."
    fMethodRef = undefined
