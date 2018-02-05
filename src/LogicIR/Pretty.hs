-- Very crude pretty printer for debugging, should not be used in production!
module LogicIR.Pretty (prettyLExpr) where

import Data.List
import Data.Maybe
import qualified Data.Map as M
import Z3.Monad
import Z3.Opts

import LogicIR.Expr
import LogicIR.Fold

prettyLExpr :: LExpr -> String
prettyLExpr = foldLExpr prettyLExprAlgebra

prettyType :: Type -> String
prettyType (TPrim PInt) = "int"
prettyType (TPrim PBool)  = "bool"
prettyType (TPrim PReal)  = "real"
prettyType (TArray t)     = "[" ++ prettyType t ++ "]"

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
    CEqual   -> "=="
    CLess    -> "<"
    CGreater -> ">"

prettyNUnop :: LUnop -> String
prettyNUnop op =
  case op of
    NNeg -> "-"
    LNot -> "!"

prettyVar :: Var -> String
prettyVar (Var t n) = n ++ ":" ++ prettyType t

prettyLExprAlgebra :: LExprAlgebra String
prettyLExprAlgebra = (fConst, prettyVar, fUnop, fBinop, fIf, fQuant, fArray, fIsnull, fLen) where
    fConst c = case c of
                    CBool b -> if b then "true" else "false"
                    CInt n  -> show n
                    CReal n -> show n
                    CNil    -> "nil"
    fUnop o a = prettyNUnop o ++ a
    fBinop a o b = a ++ " " ++ prettyLBinop o ++ " " ++ b
    fIf c a b = "(" ++ c ++ ") ? (" ++ a ++ ") : (" ++ b ++ ")"
    fQuant o v d a = '(' : show o ++ " " ++ prettyVar v ++ ": " ++ d ++ ": " ++ a ++ ")"
    fArray v a = prettyVar v ++ "[" ++ a ++ "]"
    fIsnull v = "isNull(" ++ prettyVar v ++ ")"
    fLen v = "len(" ++ prettyVar v ++ ")"
