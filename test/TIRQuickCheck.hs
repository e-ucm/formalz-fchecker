module TIRQuickCheck where

import System.IO.Unsafe (unsafePerformIO)
import System.IO.Silently (silence)
import Test.HUnit

import SimpleFormulaChecker

edslSrc = "examples/javawlp_edsl/src/nl/uu/javawlp_edsl/Main.java"

testEquiv :: Bool -> String -> String -> Int -> Assertion
testEquiv b s s' n =
  unsafePerformIO (silence $ testSpec (edslSrc, s) (edslSrc, s') n) @?= b
eq  s s' n = testEquiv True s s' n
neq s s' n = testEquiv False s s' n

quickCheckTests =
  [ (eq  "swap_spec1" "swap_spec1" 10)
  , (neq "swap_spec1" "swap_spec2" 10)
  , (neq "getMax_spec1" "getMax_spec2" 1000)
  , (neq "test1" "test2" 10)
  , (eq  "blob1" "blob1" 10)
  , (neq "test1_" "test2" 10)
  , (neq "null1" "null2" 10)
  , (neq "swap_spec1" "swap_spec3" 10)
  , (neq "swap_spec1" "swap_spec4" 100)
  , (neq "null3" "test2" 10)
  , (neq "sorted1" "test2" 10)
  , (neq "sorted1" "sorted2" 1000)
  , (neq "sorted1" "sorted3" 1000)
  , (neq "test2" "sorted3" 10)
  , (neq "sorted3" "sorted4" 1000)
  , (eq  "sorted1" "sorted4" 1000)
  ]