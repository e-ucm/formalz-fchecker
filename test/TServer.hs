{-# LANGUAGE ScopedTypeVariables #-}
module TServer where

import Control.Exception  (ErrorCall (..), throwIO)
import System.IO          (stderr, stdout)
import System.IO.Silently (hSilence)

import Test.HUnit

import Network.HTTP.Client      (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (Port, testWithApplication)
import Servant.Client

import Model
import API
import Server

-- Derive client functions from server API.
compareClient :: ApiReqBody -> ClientM ApiResponse
compareClient = client compareApi

serverTests :: [Assertion]
serverTests =
  [ testWithApplication (return app) $ \port -> do
      putStrLn $ "  t" ++ show i ++ args
      let constructSrc p = "public static void t" ++ args ++ "{pre(" ++ p ++ ");}"
      let srcA = constructSrc preA
      let srcB = constructSrc preB
      let opts = defOptions {mode = SoftDebug, checkPost = False}
      let req  = ApiReqBody srcA srcB opts
      (ApiResponse typ' _ e' fb') <- port <@ compareClient req
      case e' of
        Just e  -> error e
        Nothing -> (typ', fb') @?= (typ, fb)
  | (i :: Int, ((args, preA, preB), typ, fb)) <- zip [1..]
      [ -- 1, 2. Integer arithmetic
        (("(int x)", "x > 0", "x >= 1"), Equiv, Nothing)
      , (("(long x)", "x == 0", "x == (10 / 10 - 1)"), Equiv, Nothing)
        -- 3, 4. Real arithmetic
      , (("(float x)", "true", "0 == 0.0"), Equiv, Nothing)
      , (("(double x)", "(x * 1.1) == 0.0", "(x * 2) == (1 - 1)"), Equiv, Nothing)
       -- 5. Simple feedback
      , (("()", "true", "false || false")
        , NotEquiv, Just defFeedback' { pre = (False, True, False, False) })
      , -- 6. Example with 1D int arrays (TODO fix test backend)
        (( "(int[] a)"
         , "imp(a.length == 1, forall(a, i -> a[i] > 0))"
         , "imp((1 + a.length) == 2 , a[0] >= 1)"
         ), Equiv , Nothing)
        -- 7. Example with 1D real arrays (TODO fix)
      , (( "(double[] a)"
         , "imp(a.length == 1, forall(a, i -> a[i] > 0.1))"
         , "imp((1 + a.length) == 2, forall(a, i -> a[i] > (0.2 - 0.1)))" -- a[0] > (2.1 - 2.0))
         ), Equiv, Nothing)
        -- 8. Reported bug (TODO fix test backend)
      , (( "(int[] a, int i, int j)"
         , "false"
         , "a[j] == oldai && a[i] == oldaj"
         )
        , NotEquiv, Just defFeedback' { pre  = (False, False, True, True) })
      ]
  ]
  -- ++ TODO check errors as well (type-checking, etc...)

(<@) :: Port -> ClientM a -> IO a
port <@ action = hSilence [stdout, stderr] $ do
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl Http "localhost" port ""
  result <- runClientM action (ClientEnv manager baseUrl)
  case result of
    Left e   -> throwIO $ ErrorCall $ show e
    Right a  -> return a
