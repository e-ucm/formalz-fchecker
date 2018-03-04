module TEquivalenceClasses where

import Control.Monad
import Data.List          (elemIndex)
import Data.List.Split    (splitOn)
import Data.Maybe
import System.IO          (stderr, stdout)
import System.IO.Silently (hSilence)
import System.IO.Unsafe   (unsafePerformIO)
import Test.HUnit

import API
import Javawlp.Engine.HelperFunctions (parseMethodIds)
import Model

testEquiv :: Response -> String -> String -> String -> Assertion
testEquiv b src s s' =
  (case unsafePerformIO (hSilence [stdout, stderr] $ compareSpec Debug File (src, s) (src, s')) of
    NotEquivalent x -> NotEquivalent emptyModel
    x               -> x
   ) @?= b
(.==) = testEquiv Equivalent
(.!=) = testEquiv $ NotEquivalent emptyModel

genEquivTests edslSrc =
  let methodIds = unsafePerformIO (hSilence [stdout, stderr] $ parseMethodIds edslSrc)
      getClass = last . splitOn "_"
      tailFrom :: Eq a => [a] -> a -> [a]
      tailFrom xs x = case elemIndex x xs of Just i  -> snd $ splitAt i xs
                                             Nothing -> []
  in [ a `op` b | a <- methodIds
                , b <- methodIds `tailFrom` a
                , a /= b
                , let op = unsafePerformIO $ do
                        putStrLn $ "  (" ++ a ++ testOpS ++ b ++ ")"
                        return $ testOp edslSrc
                        where [clA, clB] = getClass <$> [a, b]
                              eq = clA == clB
                              (testOp, testOpS) =
                                if eq then ((.==), " == ") else ((.!=), " != ")
                ]
