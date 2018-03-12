module LogicIR.Pretty (prettyLExpr) where

import LogicIR.Expr
import LogicIR.Fold

prettyLExpr :: LExpr -> String
prettyLExpr = foldLExpr prettyLExprAlgebra

prettyType :: Type -> String
prettyType (TPrim PInt)  = "int"
prettyType (TPrim PBool) = "bool"
prettyType (TPrim PReal) = "real"
prettyType (TArray t)    = "[" ++ prettyType t ++ "]"

prettyLBinop :: LBinop -> String
prettyLBinop op =
  case op of
    NAdd     -> "+"
    NSub     -> "-"
    NMul     -> "*"
    NDiv     -> "/"
    NRem     -> "%"
    LAnd     -> "&&"
    LOr      -> "||"
    LImpl    -> "->"
    LEqual   -> "<->"
    CEqual   -> "=="
    CLess    -> "<"
    CGreater -> ">"

prettyNUnop :: LUnop -> String
prettyNUnop op =
  case op of
    NNeg -> "-"
    LNot -> "!"

prettyVar :: Var -> String
prettyVar (Var t x) = x ++ ":" ++ prettyType t

prettyLExprAlgebra :: LAlgebra String
prettyLExprAlgebra = LAlgebra fConst prettyVar fUnop fBinop fIf fQuant fArray fIsnull fLen
  where
    fConst c = case c of
                    CBool x -> if x then "true" else "false"
                    CInt x  -> show x
                    CReal x -> show x
                    CNil    -> "nil"
    fUnop o a = prettyNUnop o ++ a
    fBinop x o y = x ++ " " ++ prettyLBinop o ++ " " ++ y
    fIf c x y = "(" ++ c ++ ") ? (" ++ x ++ ") : (" ++ y ++ ")"
    fQuant o x d a = '(' : show o ++ " " ++ prettyVar x ++ ": " ++ d ++ ": " ++ a ++ ")"
    fArray x a = prettyVar x ++ "[" ++ a ++ "]"
    fIsnull x = "isNull(" ++ prettyVar x ++ ")"
    fLen x = "len(" ++ prettyVar x ++ ")"
