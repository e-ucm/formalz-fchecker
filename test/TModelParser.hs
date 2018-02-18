{-# LANGUAGE OverloadedStrings #-}
module TModelParser where
import Test.HUnit

import Data.String
import Model

modelParserTests =
  [ "true" @?= BoolVal True
  , "false" @?= BoolVal False
  , "1" @?= IntVal 1
  , "1.0" @?= RealVal 1.0
  , "(- 1.0)" @?= RealVal (-1.0)
  , "(- (/ 1.0 2.0))" @?= RealVal (-0.5)
  ]
