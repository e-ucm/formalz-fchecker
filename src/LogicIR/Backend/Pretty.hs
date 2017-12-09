-- Very crude pretty printer for debugging, should not be used in production!

module LogicIR.Backend.Pretty (prettyLExpr) where

import Data.List

import LogicIR.Expr
import LogicIR.Fold

prettyLExpr :: LExpr -> String
prettyLExpr = foldLExpr prettyLExprAlgebra

prettyType :: Type -> String
prettyType (TPrim PInt32) = "int"
prettyType (TPrim PBool) = "bool"
prettyType (TArray t) = prettyType t ++ "[]"

prettyLBinop :: LBinop -> String
prettyLBinop NAdd = "+"
prettyLBinop NSub = "-"
prettyLBinop NMul = "*"
prettyLBinop NDiv = "/"
prettyLBinop NRem = "%"
prettyLBinop NShl = ">>"
prettyLBinop NShr = "<<"
prettyLBinop NAnd = "&"
prettyLBinop NOr = "|"
prettyLBinop NXor = "^"
prettyLBinop LAnd = "&&"
prettyLBinop LOr = "||"
prettyLBinop LImpl = "->"
prettyLBinop CEqual = "=="
prettyLBinop CNEqual = "!="
prettyLBinop CLess = "<"
prettyLBinop CGreater = ">"
prettyLBinop CLeq = "<="
prettyLBinop CGeq = ">="

prettyNUnop :: LUnop -> String
prettyNUnop NNeg = "-"
prettyNUnop NNot = "~"
prettyNUnop LNot = "!"

prettyVar :: Var -> String
prettyVar (Var t n) = prettyType t ++ ":" ++ n

prettyLExprAlgebra :: LExprAlgebra String
prettyLExprAlgebra = (fConst, prettyVar, fUnop, fBinop, fIf, fQuant, fArray, fIsnull, fLen) where
    fConst c = case c of
                    CBool b -> if b then "true" else "false"
                    CInt n -> show n
                    CNil -> "nil"
    fUnop o a = prettyNUnop o ++ a
    fBinop a o b = a ++ " " ++ prettyLBinop o ++ " " ++ b
    fIf c a b = "(" ++ c ++ ") ? (" ++ a ++ ") : (" ++ b ++ ")"
    fQuant o v d a = '(' : show o ++ " " ++ prettyVar v ++ ": " ++ d ++ ": " ++ a ++ ")"
    fArray v a = prettyVar v ++ "[" ++ a ++ "]"
    fIsnull v = "isNull(" ++ prettyVar v ++ ")"
    fLen v = "len(" ++ prettyVar v ++ ")"
