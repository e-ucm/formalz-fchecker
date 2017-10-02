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

javaExpToLExprAlgebra :: ExpAlgebra (TypeEnv -> [TypeDecl] -> LExpr)
javaExpToLExprAlgebra = (fLit, fClassLit, fThis, fThisClass, fInstanceCreation, fQualInstanceCreation, fArrayCreate, fArrayCreateInit, fFieldAccess, fMethodInv, fArrayAccess, fExpName, fPostIncrement, fPostDecrement, fPreIncrement, fPreDecrement, fPrePlus, fPreMinus, fPreBitCompl, fPreNot, fCast, fBinOp, fInstanceOf, fCond, fAssign, fLambda, fMethodRef) where
    fLit lit _ _ = case lit of -- TODO: support more type literals
                    Boolean b -> LConst b
                    Int n -> NConst (fromIntegral n) -- TODO: use Integer in LExpr?
                    Null -> undefined -- TODO: null support
                    _ -> error $ show $ lit
    fClassLit = error "fClassLit not supported..."
    fThis = error "fThis not supported..."
    fThisClass = error "fThisClass not supported..."
    fInstanceCreation = error "fInstanceCreation not supported..."
    fQualInstanceCreation = error "fQualInstanceCreation not supported..."
    fArrayCreate = undefined
    fArrayCreateInit = undefined
    fFieldAccess = undefined
    fMethodInv = undefined
    fArrayAccess arrayIndex env decls = case arrayIndex of -- TODO: better type checking + multiple dimension arrays + better abstractions
                                             ArrayIndex (ExpName name) [ExpName index] ->
                                                let (arrayType, indexType) = (lookupType decls env name, lookupType decls env index) in
                                                    case arrayType of
                                                         (RefType (ArrayType (PrimType IntT))) ->
                                                            case indexType of
                                                                 PrimType IntT -> NArray (Var (TPrim PInt) (prettyPrint name)) [NVar (Var (TPrim PInt) (prettyPrint index))]
                                                                 _ -> error $ show (arrayIndex, indexType)
                                                         _ -> error $ show (arrayIndex, arrayType)
    fExpName name env decls = let symbol = prettyPrint name in let t = lookupType decls env name in
                                    -- If we're not dealing with library methods, we should be able to get the type from the type environment
                                    case t of
                                        PrimType BooleanT    -> LVar (Var (TPrim PBool) symbol)
                                        PrimType IntT        -> NVar (Var (TPrim PInt) symbol)
                                        t                    -> error ("Verifier: Type of " ++ prettyPrint name ++ " unknown or not implemented: " ++ show t)
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
    fLambda = undefined -- TODO: see if this should be ignored and handled in function call instead...
    fMethodRef = undefined