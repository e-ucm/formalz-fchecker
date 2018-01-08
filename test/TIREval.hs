module TIREval where

import System.IO.Unsafe (unsafePerformIO)
import System.IO.Silently (silence)
import Test.HUnit

import SimpleFormulaChecker

edslSrc = "examples/javawlp_edsl/src/nl/uu/javawlp_edsl/Main.java"

testEval :: (Bool, Maybe Bool) -> String -> Assertion
testEval t@(evalPoss, evals2True) s =
  unsafePerformIO (silence $ evaluate (edslSrc, s)) @?= t
(.==) = testEval (True, Just True)
(.=!) = testEval (True, Just False)
(.!)  = testEval (False, Nothing)

evalTests =
  [ (.==) "simple_eval1"
  , (.=!) "simple_eval2"
  , (.!)  "simple_eval3"
  ]
