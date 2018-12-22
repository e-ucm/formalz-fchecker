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
  -- TODO: change mode to SoftDebug
  res <- hSilence [stdout, stderr] $
          compareSpec (Options Z3 File True True) (src, s) (src, s')
  toDefault res @?= b

(.==), (.!=), (.??) :: String -> String -> Assertion
(.==) = testEquiv (Equivalent defFeedback')
(.!=) = testEquiv $ NotEquivalent emptyModel defFeedback'
(.??) = testEquiv Undefined

examples :: [Assertion]
examples =
  [ "empty1" .== "empty2" -- #1
  , "arr1" .!= "arr2" -- #2
  , "swap_spec1" .== "swap_spec1" -- #3
  , "swap_spec1" .!= "swap_spec2" -- #4
  , "getMax_spec1" .!= "getMax_spec2" -- #5
  , "test1" .!= "test2" -- #6
  , "blob1" .== "blob1" -- #7
  , "test1_" .!= "test2" -- #8
  , "null1" .!= "null2" -- #9
  , "swap_spec1" .!= "swap_spec3" -- #10
  , "swap_spec1" .!= "swap_spec4" -- #11
  , "null3" .!= "test2" -- #12
  , "sorted1" .!= "test2" -- #13
  , "sorted1" .!= "sorted2" -- #14
  , "sorted2".!= "sorted3" -- #15
  , "sorted1".!= "sorted3" -- #16
  , "test2" .!= "sorted3" -- #17
  , "sorted3" .!= "sorted4" -- #18
  , "sorted1" .== "sorted4" -- #19
  , "imp1" .!= "imp2" -- #20
  , "varIntro11" .!= "varIntro12" -- #21 TODO fix test backend
  , "varIntro21" .== "varIntro22" -- #22
  , "varIntro31" .== "varIntro32" -- #23
  , "varIntro41" .== "varIntro42" -- #24 TODO fix test backend
  , "varIntro51" .== "varIntro52" -- #25
  , "varIntro61" .== "varIntro62" -- #26
  , "array2d11" .== "array2d12" -- #27 TODO fix test backend
  , "arr11" .== "arr12" -- #28 TODO fix test backend
  , "eq11" .== "eq12" -- #29
  , "real21" .== "real22" -- #30
  , "array2d21" .!= "array2d22" -- #31 TODO fix test backend
  , "swap2d" .!= "sort2d" -- #32
  , "max2d_1" .== "max2d_2a" -- #33
  , "max2d_1" .== "max2d_2b" -- #34
  , "max2d_2a" .== "max2d_2b" -- #35
  , "sortbylength_1" .== "sortbylength_2a" -- #36
  , "sortbylength_1" .== "sortbylength_2b" -- #37
  , "sortbylength_2a" .== "sortbylength_2b" -- #38
  ]
