module LogicIR.Pretty (prettyLExpr) where

import LogicIR.Expr
import LogicIR.Fold

prettyLExpr :: LExpr -> String
prettyLExpr =
  foldLExpr $ LAlgebra fConst prettyVar fUnop fBinop fIf fQuant fArray fIsnull fLen
  where
    fConst c = case c of
      CBool x -> if x then "true" else "false"
      CInt x  -> show x
      CReal x -> show x
      CNil    -> "nil"
    fUnop o a = prettyNUnop o ++ parens a
    fBinop x o y = "(" ++ x ++ " " ++ prettyLBinop o ++ " " ++ y ++ ")"
    fIf c x y = parens c ++ "?" ++ parens x ++ ":" ++ parens y
    fQuant o x d a = parens $ prettyQuant o ++ " " ++ prettyVar x ++ " :: " ++ d ++ " -> " ++ a
    fArray x a = prettyVar x ++ "[" ++ a ++ "]"
    fIsnull x = "isNull" ++ parens (prettyVar x)
    fLen x = "len" ++ parens (prettyVar x)

    parens s = "(" ++ s ++ ")"
    prettyVar :: Var -> String
    prettyVar (Var t x) = x ++ ":" ++ prettyType t
    prettyQuant :: QOp -> String
    prettyQuant QAll = "forall"
    prettyQuant QAny = "exists"
    prettyNUnop :: LUnop -> String
    prettyNUnop NNeg = "-"
    prettyNUnop LNot = "!"
    prettyLBinop :: LBinop -> String
    prettyLBinop op = case op of
        NAdd     -> "+"
        NSub     -> "-"
        NMul     -> "*"
        NDiv     -> "/"
        NRem     -> "%"
        LAnd     -> "/\\"
        LOr      -> "\\/"
        LImpl    -> "==>"
        LEqual   -> "<==>"
        CEqual   -> "=="
        CLess    -> "<"
        CGreater -> ">"
    prettyType :: Type -> String
    prettyType (TPrim PInt)  = "int"
    prettyType (TPrim PBool) = "bool"
    prettyType (TPrim PReal) = "real"
    prettyType (TArray t)    = "[" ++ prettyType t ++ "]"
