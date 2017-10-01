module LogicIR.Expr where

-- TODO: pretty printer
-- Based on my (Duncan's) previous work: https://github.com/mrexodia/wp/blob/master/Wp.hs

data Primitive = PBool
               | PInt
               deriving (Show, Eq, Read)

data Type = TPrim Primitive
          | TArray Type
          deriving (Show, Eq, Read)

-- Typed + named variable
data Var = Var Type String
         deriving (Show, Eq, Read)

-- Numeral unary operators
data NUnop = NNeg
           | NNot
           deriving (Show, Eq, Read)

-- Numeral binary operators
data NBinop = NAdd
            | NSub
            | NMul
            | NDiv
            | NRem
            | NShl
            | NShr
            | NAnd
            | NOr
            | NXor
            deriving (Show, Eq, Read)

-- Reference: https://en.wikipedia.org/wiki/First-order_logic#Logical_symbols

-- Logical operators
data LBinop = LAnd -- Conjunction
            | LOr -- Disjunction
            | LImpl -- Implication
            | LBicond -- Biconditional (TODO: remove?)
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

-- Logical expressions
data LExpr = LConst Bool -- True/False
           | LVar Var -- Variable
           | LNot LExpr -- Logical negation/not
           | LBinop LExpr LBinop LExpr -- Logical operator
           | LComp NExpr COp NExpr -- Integer comparison
           | LQuant QOp [Var] LExpr -- Logical quantifier
           | LArray Var [NExpr] -- Logical array access (TODO: remove?)

           | NConst Int -- Integer constant
           | NVar Var -- Variable
           | NUnop NUnop NExpr -- Unary operator
           | NBinop NExpr NBinop NExpr -- Binary operators
           | NArray Var [NExpr] -- Integer array access
           deriving (Show, Eq, Read)