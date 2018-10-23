{-# LANGUAGE OverloadedStrings #-}
module TNormalizer (normTests, desugarTests) where

import Test.HUnit

import Data.List (intersperse)
import Data.List.Split (splitOn)

import API
import LogicIR.Expr (LExpr)
import LogicIR.Pretty (prettyLExpr)
import LogicIR.Normalizer
import LogicIR.Parser ()
import LogicIR.Frontend.Java
import LogicIR.Preprocess (preprocess)


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

desugarTests :: [Assertion]
desugarTests =
  [ "with(b[0], b0 -> b0[0])"
      ~> "TEMP_0:[bool] == b:[[bool]][0] /\\ TEMP_0:[bool][0]"
  , "with(true, t -> with(false, f -> t || f))"
      ~> "TEMP_0:bool == false /\\ (TEMP_1:bool == true /\\ (TEMP_1:bool \\/ TEMP_0:bool))"
  , "b[0][0]"
      ~> "TEMP_0:[bool] == b:[[bool]][0] /\\ TEMP_0:[bool][0]"
  , "a[0][0] == 9"
      ~> "TEMP_0:[int] == a:[[int]][0] /\\ TEMP_0:[int][0] == 9"
  , "a[0][0] == a[1][1]"
      ~> "TEMP_0:[int] == a:[[int]][0] /\\ (TEMP_1:[int] == a:[[int]][1] /\\ TEMP_0:[int][0] == TEMP_1:[int][1])"
  , "a[0][0] == a[0][1]"
      ~> "TEMP_0:[int] == a:[[int]][0] /\\ TEMP_0:[int][0] == TEMP_0:[int][1]"
  , "forall(a, i -> a[i][0] == 1)"
      ~> "(forall i:int :: (i:int >= 0) /\\ (i:int < len(a:[[int]])) -> (TEMP_0:[int] == a:[[int]][i:int]) /\\ (TEMP_0:[int][0] == 1))"
  , "forall(a[0], i -> a[0][i] == 1)"
      ~> "((TEMP_0:[int] == a:[[int]][0]) /\\ (forall i:int :: ((i:int >= 0) /\\ (i:int < len(TEMP_0:[int]))) -> (TEMP_0:[int][i:int] == 1)))"
  , "a[0].length == 1"
      ~> "TEMP_0:[int] == a:[[int]][0] /\\ len(TEMP_0:[int]) == 1"
  , "a[0].length == a[1].length"
      ~> "TEMP_0:[int] == a:[[int]][0] /\\ (TEMP_1:[int] == a:[[int]][1] /\\ len(TEMP_0:[int]) == len(TEMP_1:[int]))"
  , "a[0] == null"
      ~> "TEMP_0:[int] == a:[[int]][0] /\\ TEMP_0:[int] == null"
  , "forallr(a[0], 1, a[0].length, i -> a[0][i] == 0)"
      ~> "TEMP_0:[int] == a:[[int]][0] /\\ (forall i:int :: (i:int >= 1) /\\ (i:int < len(TEMP_0:[int])) -> (TEMP_0:[int][i:int] == 0))"
  , "imp(a[0] != null, a[0].length == 1)"
      ~> "TEMP_0:[int] == a:[[int]][0] /\\ ((TEMP_0:[int] != null) ==> (len(TEMP_0:[int]) == 1))"
  ]

(~>) :: Source -> LExpr -> Assertion
src ~> lexpr = do
  m@(decls, _, env) <- parseMethod Raw src'
  javaExpToLExpr (extractExpr $ getMethodCalls m "pre") env decls .== lexpr
  where
    args = [ "int x", "float f", "double d", "int[][] a", "boolean[][] b"]
    src' = ( "public class Main {public static void t("
             ++ concat (intersperse "," args)
             ++ ") {pre(" ++ src ++ " );}}"
           , "t"
           )
    (.==) :: LExpr -> LExpr -> Assertion
    l .== l' = prettyLExpr (preprocess l) @?= prettyLExpr (preprocess l')
