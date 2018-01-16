module TIRQuickCheck where

import System.IO.Unsafe (unsafePerformIO)
import System.IO.Silently (silence)
import Test.HUnit

import SimpleFormulaChecker

edslSrc = "examples/javawlp_edsl/src/nl/uu/javawlp_edsl/Main.java"

testEquiv :: Bool -> String -> String -> Assertion
testEquiv b s s' =
  unsafePerformIO (silence $ testSpec (edslSrc, s) (edslSrc, s') 100000) @?= b
(.==) = testEquiv True
(.!=) = testEquiv False

quickCheckTests =
  [ "swap_spec1" .== "swap_spec1"
  , "swap_spec1" .!= "swap_spec2"
  , "getMax_spec1" .!= "getMax_spec2"
  , "test1" .!= "test2"
  , "blob1" .== "blob1"
  , "test1_" .!= "test2"
  , "null1" .!= "null2"
  , "swap_spec1" .!= "swap_spec3"
  , "swap_spec1" .!= "swap_spec4"
  , "null3" .!= "test2"
  , "sorted1" .!= "test2"
  , "sorted1" .!= "sorted2"
  , "sorted1".!= "sorted3"
  , "test2" .!= "sorted3"
  , "sorted3" .!= "sorted4"
  , "sorted1" .== "sorted4"
  ]
