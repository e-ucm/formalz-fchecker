module TExamples where
import System.IO.Unsafe (unsafePerformIO)
import System.IO.Silently (silence)
import Test.HUnit

import SimpleFormulaChecker

src = "examples/javawlp_edsl/src/nl/uu/javawlp_edsl/Main.java"

testEquiv :: Response -> String -> String -> Assertion
testEquiv b s s' =
  (case unsafePerformIO (silence $ compareSpec (src, s) (src, s')) of
    NotEquivalent _ -> NotEquivalent Nothing
    x -> x
   ) @?= b
(.==) = testEquiv Equivalent
(.!=) = testEquiv $ NotEquivalent Nothing
(.??) = testEquiv Timeout

examples =
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
  , "sorted1" .!= "sorted4"
  ]
