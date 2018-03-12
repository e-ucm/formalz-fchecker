{-# LANGUAGE OverloadedStrings #-}
module TNormalizer (normTests) where

import Test.HUnit

import LogicIR.Expr       ()
import LogicIR.Normalizer
import LogicIR.Parser     ()
import LogicIR.Pretty

normTests :: [Assertion]
normTests =
  [ toNNF "a:int == 0 <==> a:int <= 0 /\\ a:int >= 0" @?=
      "(!(a:int == 0) \\/ (a:int <= 0 /\\ a:int >= 0)) /\\ (((!(a:int < 0) /\\ \
      \ !(a:int == 0)) \\/ (!(a:int > 0) /\\ !(a:int == 0))) \\/ (a:int == 0))"
  , toCNF "a:int == 0 <==> a:int < 0 /\\ a:int > 0" @?=
      "((!(a:int == 0) \\/ (a:int < 0)) /\\ (!(a:int == 0) \\/ (a:int > 0))) \
      \ /\\ ((!(a:int < 0) \\/ !(a:int > 0)) \\/ (a:int == 0))"
  , toCNF "true /\\ (forall x:int :: x:int >= 1 -> x:int + 1 > 1)" @?=
    "(forall x:int :: x:int >= 1 -> (true /\\ x:int + 1 > 1))"
  , toCNF "(forall a:int :: a:int == 0 <==> a:int < 0 /\\ a:int > 0 -> \
      \ (exists x:int :: true \\/ (false /\\ false) -> x:int + a:int == 0))" @?=
      "(forall a:int :: \
      \ ((!(a:int == 0) \\/ (a:int < 0)) /\\ (!(a:int == 0) \\/ (a:int > 0))) \
      \ /\\ ((!(a:int < 0) \\/ !(a:int > 0)) \\/ (a:int == 0)) \
      \ -> \
      \ (exists x:int :: (true \\/ false) /\\ (true \\/ false) -> x:int + a:int == 0))"
  ]
