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
    fLit lit _ _ = case lit of
                    Boolean b -> LConst b
                    Int n -> NConst (fromIntegral n) -- TODO: use Integer in LExpr?
                    _ -> error $ show $ lit
    fClassLit = undefined
    fThis = undefined
    fThisClass = undefined
    fInstanceCreation = undefined
    fQualInstanceCreation = undefined
    fArrayCreate = undefined
    fArrayCreateInit = undefined
    fFieldAccess = undefined
    fMethodInv = undefined
    fArrayAccess arrayIndex env decls = case arrayIndex of -- TODO: better type checking + multiple dimension arrays
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
    fPostIncrement = undefined
    fPostDecrement = undefined
    fPreIncrement = undefined
    fPreDecrement = undefined
    fPrePlus e env decls = e env decls
    fPreMinus e env decls = NUnop NNeg (e env decls)
    fPreBitCompl = undefined
    fPreNot = undefined
    fCast = undefined
    fBinOp e1 op e2 env decls = let (a, b) = (e1 env decls, e2 env decls) in -- TODO: type checking
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
    fCond = undefined
    fAssign = undefined
    fLambda = undefined
    fMethodRef = undefined