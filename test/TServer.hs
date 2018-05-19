module TServer where

import Control.Exception  (ErrorCall (..), throwIO)
import System.IO          (stderr, stdout)
import System.IO.Silently (hSilence)

import Test.HUnit

import Network.HTTP.Client      (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (Port, testWithApplication)
import Servant.Client

import Model
import Server

-- Derive client functions from server API.
compareClient :: ApiReqBody -> ClientM ApiResponse
compareClient = client compareApi

serverTests :: [Assertion]
serverTests =
  [ testWithApplication (return app) $ \port -> do
      putStrLn $ "  [" ++ funA ++ " ?? " ++ funB ++ "]"
      let srcA = constructSrc funA preA postA
      let srcB = constructSrc funB preB postB
      (ApiResponse typ' _ e' fb') <- port <@ compareClient (ApiReqBody srcA srcB)
      case e' of
        Just e  -> error e
        Nothing -> (typ', fb') @?= (typ, fb)
  | ( (funA, preA, postA), (funB, preB, postB), typ, fb ) <-
      [ -- Simple equivalent specifications
        ( ("t11(int x)", "x > 0",  "x == 0")
        , ("t12(int x)", "x >= 1", "x == (10 / 10 - 1)")
        , Equiv
        , Nothing
        )
      , -- Simple feedback
        ( ("t21()", "true",           "false")
        , ("t22()", "false || false", "false && true")
        , NotEquiv
        , Just Feedback { pre  = (False, True,  False, False)
                        , post = (False, False, False, True) }
        )
      , -- Example with reals
        ( ("t31(float x)", "x > 0.0",     "(x * 1.1) == 0.0")
        , ("t32(float x)", "x > (1 - 1)", "(x * 2)   == (1 - 1)")
        , Equiv
        , Nothing
        )
      , -- Example with 1D int arrays
        ( ("t41(int[] a)", "true",     "imp(a.length == 1, forall(a, i -> a[i] > 0))")
        , ("t42(int[] a)", "0 == 0.0", "imp((1 + a.length) == 2 , a[0] >= 1)")
        , Equiv
        , Nothing
        )
      , -- Example with 1D real arrays (TODO fix)
        ( ("t51(double[] a)", "true", "imp(a.length == 1, forall(a, i -> a[i] > 0.1))")
        , ("t52(double[] a)", "true", "imp((1 + a.length) == 2, a[0] > (2.1 - 2.0))")
        , Equiv
        , Nothing
        )
      , -- Reported bug (TODO fix)
        ( ("t61(int[] a, int i, int j)", "i >= 0 && j >= 0", "false")
        , ("t62(int[] a, int i, int j)", "i >= 0 && j >= 0", "a[j] == oldai && a[i] == oldaj")
        , NotEquiv
        , Just Feedback { pre  = (True,  False, True, False)
                        , post = (False, False, True, True) }
        )
      ]
  ]
  -- ++ TODO check errors as well (type-checking, etc...)
  where
    constructSrc fun preP postP = "public static void " ++ fun ++ "{ pre("
                                    ++ preP ++ "); post(" ++ postP ++ "); }"

(<@) :: Port -> ClientM a -> IO a
port <@ action = hSilence [stdout, stderr] $ do
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl Http "localhost" port ""
  result <- runClientM action (ClientEnv manager baseUrl)
  case result of
    Left e   -> throwIO $ ErrorCall $ show e
    Right a  -> return a
