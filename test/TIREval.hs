module TIREval where

import System.IO.Unsafe (unsafePerformIO)
import System.IO.Silently (silence)
import Test.HUnit

import SimpleFormulaChecker

edslSrc = "examples/javawlp_edsl/src/nl/uu/javawlp_edsl/Main.java"

testEval :: Bool -> String -> Assertion
testEval b s =
  unsafePerformIO (silence $ evaluate (edslSrc, s)) @?= b
(.=) = testEval True
(.!) = testEval False

evalTests =
  [ (.=) "simple_eval1"
  , (.!) "simple_eval2"
  ]
