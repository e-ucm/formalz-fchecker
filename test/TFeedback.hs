{-# LANGUAGE OverloadedStrings #-}
module TFeedback where
import System.IO          (stderr, stdout)
import System.IO.Silently (hSilence)
import System.IO.Unsafe   (unsafePerformIO)
import Test.HUnit

import LogicIR.Expr
import LogicIR.Parser
import LogicIR.Backend.Z3.API
import Model

testEquiv :: Response -> LExpr -> LExpr -> Assertion
testEquiv b s s' = do
  res <- hSilence [stdout, stderr] $ equivalentTo s s'
  (case res of
    NotEquivalent _ f -> NotEquivalent emptyModel f
    x                 -> x) @?= b
(!!=) s s' f = testEquiv (NotEquivalent emptyModel (f, NoFeedback)) s s'

feedbackTests =
  [ ("true /\\ x:int == 1" !!= "false \\/ x:int >= 1") Stronger
  , ("true /\\ x:int >= 1" !!= "false \\/ x:int == 1") Weaker
  , ("x:int == 1 /\\ y:int == 1" !!= "x:int == 2 /\\ y:int == 1") NoFeedback
  , ("x:[int][0] != 0 /\\ x:[int][1] != 0"
      !!= "x:[int][0] != 0 \\/ x:[int][1] != 0") Stronger
  ]
