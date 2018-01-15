module TEquivalenceClasses where
import System.IO.Unsafe (unsafePerformIO)
import System.IO.Silently (silence)
import Data.List.Split (splitOn)
import Test.HUnit

import SimpleFormulaChecker (compareSpec, parseMethodIds)

edslSrc = "examples/javawlp_edsl/src/nl/uu/javawlp_edsl/Test.java"

testEquiv :: Bool -> String -> String -> Assertion
testEquiv b s s' =
  unsafePerformIO (silence $ compareSpec (edslSrc, s) (edslSrc, s')) @?= b
(.==) = testEquiv True
(.!=) = testEquiv False

equivClassesTests =
  let methodIds = unsafePerformIO (silence $ parseMethodIds edslSrc)
      getClass = last . splitOn "_"
  in [ a `op` b | a <- methodIds
                , b <- methodIds
                , a /= b
                , let op = if getClass a == getClass b then (.==) else (.!=)
                ]
