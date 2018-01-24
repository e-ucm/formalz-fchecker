{-# LANGUAGE OverloadedStrings #-}
module TIRParser where

import System.IO.Unsafe (unsafePerformIO)
import Test.HUnit

import LogicIR.Expr
import LogicIR.Parser

parserTests =
  [ "1 + 1" @?= n 1 .+ n 1
  , "(1 == 1 ? 1 : 2)" @?=
      LIf (n 1 .== n 1) (n 1) (n 2)
  , "true \\/ (1 == 1 ? 1 : 2)" @?=
      b True .|| LIf (n 1 .== n 1) (n 1) (n 2)
  , "true \\/ (1 == 1 ? 1 : 2)" @?=
      b True .|| LIf (n 1 .== n 1) (n 1) (n 2)
  , "(forall x:int :: x:int >= 1 -> x:int + 1 > 1)" @?=
      forall x (v x .>= n 1) (x' .+ n 1 .> n 1)
  , "(exists x:int :: x:int >= 1 -> x:int + 1 > 1)" @?=
      exists x (v x .>= n 1) (x' .+ n 1 .> n 1)
  , "(exists x:[int] :: true -> x:[int][5] > 1)" @?=
      exists xArr (b True) ((xArr .! n 5) .> n 1)
  , "len(x:[int]) > 1 + 1" @?=
      LLen xArr .> n 1 .+ n 1
  , "len(x:[int]) == 0 /\\ ! x:[int] == null" @?=
      LLen xArr .== n 0 .&& lnot (LIsnull xArr)
  , "len(x:[real]) == 0 /\\ ! x:[real] == null" @?=
      LLen xArr' .== n 0 .&& lnot (LIsnull xArr')
  ]
  where x = var "x" "int"
        x' = v x
        xArr = var "x" "[int]"
        xArr' = var "x" "[real]"
