module TExamples where
import System.IO          (stderr, stdout)
import System.IO.Silently (hSilence)
import Test.HUnit

import API
import Model

src :: String
src = "impress_edsl/src/nl/uu/impress/Main.java"

testEquiv :: Response -> String -> String -> Assertion
testEquiv b s s' = do
  res <- hSilence [stdout, stderr] $ compareSpec Release File (src, s) (src, s')
  (case res of
    NotEquivalent _ _ -> NotEquivalent emptyModel defFeedback'
    x                 -> x) @?= b

(.==), (.!=), (.??) :: String -> String -> Assertion
(.==) = testEquiv Equivalent
(.!=) = testEquiv $ NotEquivalent emptyModel defFeedback'
(.??) = testEquiv Timeout

examples :: [Assertion]
examples =
  [ "empty1" .== "empty2"
  , "arr1" .!= "arr2"
  , "swap_spec1" .== "swap_spec1"
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
  , "sorted2".!= "sorted3"
  , "sorted1".!= "sorted3"
  , "test2" .!= "sorted3"
  , "sorted3" .!= "sorted4"
  , "sorted1" .== "sorted4"
  , "imp1" .!= "imp2"
  , "varIntro11" .!= "varIntro12"
  , "varIntro21" .== "varIntro22"
  , "varIntro31" .== "varIntro32"
  ]
