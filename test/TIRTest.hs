module TIRTest where
import System.IO.Unsafe (unsafePerformIO)
import System.IO.Silently (silence)
import Test.HUnit

import LogicIR.Backend.Z3.Model
import SimpleFormulaChecker

src = "examples/javawlp_edsl/src/nl/uu/javawlp_edsl/Main.java"

testEquiv :: Response -> String -> String -> Assertion
testEquiv b s s' =
  (case unsafePerformIO (silence $ testSpec (src, s) (src, s')) of
    NotEquivalent _ -> NotEquivalent emptyZ3Model
    x -> x
   ) @?= b
eq = testEquiv Equivalent
neq = testEquiv $ NotEquivalent emptyZ3Model

testExamples =
  [ neq "arr1" "arr2"
  , eq  "swap_spec1" "swap_spec1"
  , neq "swap_spec1" "swap_spec2"
  , neq "getMax_spec1" "getMax_spec2"
  , neq "test1" "test2"
  , eq  "blob1" "blob1"
  , neq "test1_" "test2"
  , neq "null1" "null2"
  , neq "swap_spec1" "swap_spec3"
  , neq "swap_spec1" "swap_spec4"
  , neq "null3" "test2"
  , neq "sorted1" "test2"
  , neq "sorted1" "sorted2"
  , neq "sorted2" "sorted3"
  , neq "sorted1" "sorted3"
  , neq "test2" "sorted3"
  , neq "sorted3" "sorted4"
  , eq  "sorted1" "sorted4"
  ]
