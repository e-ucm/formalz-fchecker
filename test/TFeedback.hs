{-# LANGUAGE OverloadedStrings #-}
module TFeedback where

import Control.Monad      (when)
import System.IO          (stderr, stdout)
import System.IO.Silently (hSilence)
import Test.HUnit

import qualified LogicIR.Backend.QuickCheck.API as Test
import qualified LogicIR.Backend.Z3.API         as Z3
import           LogicIR.Expr
import           LogicIR.Parser                 ()
import           Model

testEquiv :: Response -> LExpr -> LExpr -> Assertion
testEquiv expectedRes s s' = do
  res <- getResponse Z3.equivalentTo
  res' <- getResponse Test.equivalentTo
  when (res /= res') $
    error $ "Different backend responses: " ++ show res ++ " /= " ++ show res'
  res @?= expectedRes
  where
    getResponse :: (LExpr -> LExpr -> IO Response) -> IO Response
    getResponse equiv = removeModel <$> hSilence [stdout, stderr] (equiv s s')
    removeModel :: Response -> Response
    removeModel (NotEquivalent _ f) = NotEquivalent emptyModel f
    removeModel m                   = m
(!!=) :: LExpr -> LExpr -> SingleFeedback -> Assertion
(s !!= s') f = testEquiv (NotEquivalent emptyModel (Feedback f defFeedback)) s s'

feedbackTests :: [Assertion]
feedbackTests =
  [ ("true /\\ x:int == 1" !!= "false \\/ x:int >= 1") (True, False, True, True)
  , ("true /\\ x:int >= 1" !!= "false \\/ x:int == 1") (True, True, False, True)
  , ("x:int == 1 /\\ y:int == 1" !!= "x:int == 2 /\\ y:int == 1")
      (False, True, True, True) -- TODO fix test backend
  , ("x:[int][0] != 0 /\\ x:[int][1] != 0" !!= "x:[int][0] != 0 \\/ x:[int][1] != 0")
      (True, False, True, True)
  ]
