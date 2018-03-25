{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module LogicIR.Expr where

import GHC.Generics
import Control.DeepSeq

-- | The primitive types are bool and int32.
data Primitive = PBool
               | PInt
               | PReal
               deriving (Show, Eq, Read, Generic, NFData)

-- | A Type can either be a primitive or an array type.
data Type = TPrim Primitive
          | TArray Type
          deriving (Show, Eq, Read, Generic, NFData)

-- | Typed + named variable.
data Var = Var Type String
         deriving (Show, Eq, Read, Generic, NFData)

-- | Unary operators.
data LUnop = NNeg -- -n (numeric negation)
           | LNot -- !n (logical not)
           deriving (Show, Eq, Read, Generic, NFData)

-- | Binary operators.
data LBinop =
            -- numeric operators
              NAdd -- a + b
            | NSub -- a - b
            | NMul -- a * b
            | NDiv -- a / b
            | NRem -- a % b
            -- logical operators
            | LAnd -- a && b
            | LOr -- a || b
            | LImpl -- a ==> b
            | LEqual -- a <==> b
            -- comparisons
            | CEqual -- a == b
            | CLess -- a < b
            | CGreater -- a > b
            deriving (Show, Eq, Read, Generic, NFData)

-- | Quantifier operators.
data QOp = QAll | QAny
         deriving (Show, Eq, Read, Generic, NFData)

-- | Constants.
data LConst = CBool Bool
            | CInt Int
            | CReal Double
            | CNil
            deriving (Show, Eq, Read, Generic, NFData)

-- | Logical expressions.
data LExpr = LConst LConst -- constant
           | LVar Var -- variable
           | LUnop LUnop LExpr -- unary operator
           | LBinop LExpr LBinop LExpr -- binary operator
           | LIf LExpr LExpr LExpr -- if c then a else b (ternary operator)
           | LQuant QOp Var LExpr LExpr -- quantifier (op bound domain expr)
           | LArray Var LExpr -- array access
           | LIsnull Var -- var == null
           | LLen Var -- len(array)
           deriving (Show, Eq, Read, Generic, NFData)

-- Needed for the ArrayModel key ordering in the Map in LogicIR.Backend.ModelGenerator
instance Ord Var where
  (Var _ name1) `compare` (Var _ name2) = name1 `compare` name2

-- | EDSL.
forall = LQuant QAll
exists = LQuant QAny
var x t = Var t x
v = LVar
r = LConst . CReal
n = LConst . CInt
b = LConst . CBool
nil = LConst CNil
lnot = LUnop LNot
binOp op e = LBinop e op

infix 1 .: ; infix 1 .!
infix 2 .<==> ; infix 2 .==>
infix 3 .&& ; infix 3 .||
infix 4 .== ; infix 4 .!= ; infix 4 .> ; infix 4 .< ; infix 4 .<= ; infix 4 .>=
infix 5 .* ; infix 5 ./ ; infix 5 .%
infix 6 .+ ; infix 6 .-
(.:) x t = LVar $ Var t x
(.!)   = LArray
(.+)   = binOp NAdd     ; (.-)      = binOp NSub
(.*)   = binOp NMul     ; (./)      = binOp NDiv
(.%)   = binOp NRem
(.==)  = binOp CEqual   ; (.!=) x y = lnot $ x .== y
(.<)   = binOp CLess    ; (.<=) x y = x .< y .|| x .== y
(.>)   = binOp CGreater ; (.>=) x y = x .> y .|| x .== y
(.&&)  = binOp LAnd     ; (.||)     = binOp LOr
(.==>) = binOp LImpl    ; (.<==>)   = binOp LEqual
