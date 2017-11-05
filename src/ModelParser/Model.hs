module ModelParser.Model where

data FuncInst = InstInt Int Int
              | InstElse Int
              deriving (Show, Read, Eq)

data ModelVal = BoolVal Bool
              | IntVal Int
              | ArrayRef String -- TODO: immediately forward to ArrayFunc?
              | ArrayAsConst Int -- TODO: parse to InstElse?
              | ArrayFunc [FuncInst]
              deriving (Show, Read, Eq)

type Z3Model = [(String, ModelVal)]