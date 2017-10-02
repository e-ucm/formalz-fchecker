module LogicIR.Backend.Pretty (prettyLExpr) where

import Data.List

import LogicIR.Expr
import LogicIR.Fold

prettyLExpr :: LExpr -> String
prettyLExpr = foldLExpr prettyLExprAlgebra

prettyType :: Type -> String
prettyType (TPrim PInt) = "int"
prettyType (TPrim PBool) = "bool"
prettyType (TArray t) = prettyType t ++ "[]"

prettyCOp :: COp -> String
prettyCOp CEqual = "=="
prettyCOp CNEqual = "!="
prettyCOp CLess = "<"
prettyCOp CGreater = ">"
prettyCOp CLeq = "<="
prettyCOp CGeq = ">="

prettyLBinop :: LBinop -> String
prettyLBinop LAnd = "&&"
prettyLBinop LOr = "||"
prettyLBinop LImpl = "->"
prettyLBinop LBicond = "<->"

prettyNBinop :: NBinop -> String
prettyNBinop NAdd = "+"
prettyNBinop NSub = "-"
prettyNBinop NMul = "*"
prettyNBinop NDiv = "/"
prettyNBinop NRem = "%"
prettyNBinop NShl = ">>"
prettyNBinop NShr = "<<"
prettyNBinop NAnd = "&"
prettyNBinop NOr = "|"
prettyNBinop NXor = "^"

prettyNUnop :: NUnop -> String
prettyNUnop NNeg = "-"
prettyNUnop NNot = "~"

prettyVar :: Var -> String
prettyVar (Var t n) = prettyType t ++ ":" ++ n

prettyLExprAlgebra :: LExprAlgebra String
prettyLExprAlgebra = (flConst, prettyVar, flNot, flBinop, flComp, flQuant, flArray, flNil, fnConst, fnUnop, fnBinop, fnIf, fnLen) where
    flConst b = if b then "true" else "false"
    flNot a = '!' : a
    flBinop a o b = a ++ " " ++ prettyLBinop o ++ " " ++ b
    flComp a o b = a ++ " " ++ prettyCOp o ++ " " ++ b
    flQuant o v a = '(' : show o ++ " " ++ prettyVar v ++ " . " ++ a ++ ")"
    flArray v [a] = prettyVar v ++ "[" ++ (foldLExpr prettyLExprAlgebra a) ++ "]"
    flNil = "nil"
    fnConst n = show n
    fnUnop o a = prettyNUnop o ++ a
    fnBinop a o b = a ++ " " ++ prettyNBinop o ++ " " ++ b
    fnIf c a b = "(" ++ c ++ ") ? (" ++ a ++ ") : (" ++ b ++ ")"
    fnLen v = "len(" ++ prettyVar v ++ ")"