module LogicIR.Frontend.Java (javaExpToLExpr) where

import Javawlp.Engine.Folds

import Language.Java.Syntax
import Language.Java.Parser
import Language.Java.Pretty

import LogicIR.Expr

javaExpToLExpr :: Exp -> LExpr
javaExpToLExpr = foldExp javaExpToLExprAlgebra

javaExpToLExprAlgebra :: ExpAlgebra LExpr
javaExpToLExprAlgebra = (fLit, fClassLit, fThis, fThisClass, fInstanceCreation, fQualInstanceCreation, fArrayCreate, fArrayCreateInit, fFieldAccess, fMethodInv, fArrayAccess, fExpName, fPostIncrement, fPostDecrement, fPreIncrement, fPreDecrement, fPrePlus, fPreMinus, fPreBitCompl, fPreNot, fCast, fBinOp, fInstanceOf, fCond, fAssign, fLambda, fMethodRef) where
    fLit = undefined
    fClassLit = undefined
    fThis = undefined
    fThisClass = undefined
    fInstanceCreation = undefined
    fQualInstanceCreation = undefined
    fArrayCreate = undefined
    fArrayCreateInit = undefined
    fFieldAccess = undefined
    fMethodInv = undefined
    fArrayAccess = undefined
    fExpName = undefined
    fPostIncrement = undefined
    fPostDecrement = undefined
    fPreIncrement = undefined
    fPreDecrement = undefined
    fPrePlus = undefined
    fPreMinus = undefined
    fPreBitCompl = undefined
    fPreNot = undefined
    fCast = undefined
    fBinOp = undefined
    fInstanceOf = undefined
    fCond = undefined
    fAssign = undefined
    fLambda = undefined
    fMethodRef = undefined