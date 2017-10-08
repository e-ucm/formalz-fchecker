module LogicIR.Expr where

-- TODO: pretty printer
-- Based on my (Duncan's) previous work: https://github.com/mrexodia/wp/blob/master/Wp.hs

data Primitive = PBool
               | PInt32
               deriving (Show, Eq, Read)

data Type = TPrim Primitive
          | TArray Type
          deriving (Show, Eq, Read)

-- Typed + named variable
data Var = Var Type String
         deriving (Show, Eq, Read)

-- Numeral unary operators
data NUnop = NNeg -- -n (negation)
           | NNot -- ~n (binary not)
           deriving (Show, Eq, Read)

-- Numeral binary operators
data NBinop = NAdd -- a + b
            | NSub -- a - b
            | NMul -- a * b
            | NDiv -- a / b
            | NRem -- a % b
            | NShl -- a >> b
            | NShr -- a << b
            | NAnd -- a & b
            | NOr -- a | b
            | NXor -- a ^ b
            deriving (Show, Eq, Read)

-- Reference: https://en.wikipedia.org/wiki/First-order_logic#Logical_symbols

-- Logical operators
data LBinop = LAnd -- Conjunction
            | LOr -- Disjunction
            | LImpl -- Implication
            deriving (Show, Eq, Read)

-- Quantifier operators
data QOp = QAll | QAny
         deriving (Show, Eq, Read)

-- Comparison operators
data COp = CEqual -- a == b
         | CNEqual -- a != b
         | CLess -- a < b
         | CGreater -- a > b
         | CLeq -- a <= b
         | CGeq -- a >= b
         deriving (Show, Eq, Read)

type NExpr = LExpr

-- Logical expressions (TODO: clean up duplicates)
data LExpr = LConst Bool -- True/False
           | LVar Var -- Variable
           | LNot LExpr -- Logical negation/not
           | LBinop LExpr LBinop LExpr -- Logical operator
           | LComp NExpr COp NExpr -- Integer comparison
           | LQuant QOp Var LExpr -- Logical quantifier
           | LArray Var NExpr -- Logical array access
           | LNil -- Nil constant

           | NConst Int -- Integer constant (TODO: use Integer instead of Int?)
           | NUnop NUnop NExpr -- Unary operator
           | NBinop NExpr NBinop NExpr -- Binary operators
           | NIf LExpr NExpr NExpr -- if c then a else b
           | NLen Var -- len(array)
           deriving (Show, Eq, Read)