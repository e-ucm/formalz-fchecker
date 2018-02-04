module TIRTest where

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

testingModuleTests =
  [ (eq  "swap_spec1" "swap_spec1" 100)
  , (neq "swap_spec1" "swap_spec2" 100)
  , (neq "getMax_spec1" "getMax_spec2" 500)
  , (neq "test1" "test2" 100)
  , (eq  "blob1" "blob1" 100)
  , (neq "test1_" "test2" 100)
  , (neq "null1" "null2" 100)
  , (neq "swap_spec1" "swap_spec3" 100)
  , (neq "swap_spec1" "swap_spec4" 100)
  , (neq "null3" "test2" 1000)
  , (neq "sorted1" "test2" 100)
  , (neq "sorted1" "sorted2" 500)
  , (neq "sorted1" "sorted3" 100)
  , (neq "sorted3" "test2" 100)
  , (neq "sorted3" "sorted4" 500)
  , (neq  "sorted1" "sorted4" 100)
  ]