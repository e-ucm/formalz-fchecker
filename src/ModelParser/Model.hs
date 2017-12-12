module ModelParser.Model where

data FuncInst = InstInt Int Int
              | InstElse Int
              deriving (Show, Read, Eq)

data ModelVal = BoolVal Bool
              | IntVal Int
              | ArrayRef String
              | ArrayAsConst Int
              | ArrayFunc [FuncInst]
              deriving (Show, Read, Eq)

type Z3Model = [(String, ModelVal)]
