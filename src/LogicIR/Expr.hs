module LogicIR.Expr where

-- Based on previous work: https://github.com/mrexodia/wp/blob/master/Wp.hs
-- Reference: https://en.wikipedia.org/wiki/First-order_logic#Logical_symbols

-- The primitive types are bool and int32.
data Primitive = PBool
               | PInt32
               deriving (Show, Eq, Read)

-- A Type can either be a primitive or an array of Type
data Type = TPrim Primitive
          | TArray Type
          deriving (Show, Eq, Read)

-- Typed + named variable
data Var = Var Type String
         deriving (Show, Eq, Read)

-- Unary operators
data LUnop = NNeg -- -n (numeric negation)
           | NNot -- ~n (numeric binary not)
           | LNot -- !n (logical not)
           deriving (Show, Eq, Read)

-- Binary operators
data LBinop =
            -- numeric operators
              NAdd -- a + b
            | NSub -- a - b
            | NMul -- a * b
            | NDiv -- a / b
            | NRem -- a % b
            | NShl -- a >> b
            | NShr -- a << b
            | NAnd -- a & b
            | NOr -- a | b
            | NXor -- a ^ b
            -- logical operators
            | LAnd -- a && b
            | LOr -- a || b
            | LImpl -- a => b
            -- comparisons
            | CEqual -- a == b
            | CNEqual -- a != b
            | CLess -- a < b
            | CGreater -- a > b
            | CLeq -- a <= b
            | CGeq -- a >= b
            deriving (Show, Eq, Read)

-- Quantifier operators
data QOp = QAll | QAny
         deriving (Show, Eq, Read)

-- Constants
data LConst = CBool Bool
            | CInt Int
            | CNil
            deriving (Show, Eq, Read)

-- Logical expressions
data LExpr = LConst LConst -- constant
           | LVar Var -- variable
           | LUnop LUnop LExpr -- unary operator
           | LBinop LExpr LBinop LExpr -- binary operator
           | LIf LExpr LExpr LExpr -- if c then a else b (ternary operator)
           | LQuant QOp Var LExpr LExpr -- quantifier (op bound domain expr)
           | LArray Var LExpr -- array access
           | LIsnull Var -- var == null
           | LLen Var -- len(array)
           deriving (Show, Eq, Read)
