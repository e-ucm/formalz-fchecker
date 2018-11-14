module TEquivalenceClasses where

import Data.List          (elemIndex)
import Data.List.Split    (splitOn)
import System.IO          (stderr, stdout)
import System.IO.Silently (hSilence)
import System.IO.Unsafe   (unsafePerformIO)
import Test.HUnit

import API
import JavaHelpers.HelperFunctions (parseMethodIds)
import Model

testEquiv :: Response -> String -> String -> String -> Assertion
testEquiv b src s s' = do
  -- We run these tests in SoftDebug mode, because it's not feasible to expect
  -- from QuickCheck that it calculates feedback that is exactly right every time,
  -- due to the enormous search space of reals. Therefore we only check if
  -- both Z3 and QuickCheck say eq / neq, and not if they give the same feedback model.
  -- TODO: change mode to SoftDebug
  res <- hSilence [stdout, stderr] $
          compareSpec (Options Z3 File True True) (src, s) (src, s')
  toDefault res @?= b

(.==), (.!=) :: String -> String -> String -> Assertion
(.==) = testEquiv (Equivalent    defFeedback')
(.!=) = testEquiv (NotEquivalent emptyModel defFeedback')

genEquivTests :: FilePath -> [Assertion]
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
                        putStrLn $ "  (" ++ a ++ " " ++ testOpS ++ " " ++ b ++ ")"
                        return $ testOp edslSrc
                        where [clA, clB] = getClass <$> [a, b]
                              eq = clA == clB
                              (testOp, testOpS) =
                                if eq then ((.==), "==")
                                else ((.!=), "!=")
                ]
