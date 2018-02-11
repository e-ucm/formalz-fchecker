{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Server (runApi) where

import Data.Maybe
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Control.Monad.Trans (liftIO)
import Data.Vector (fromList)
import Data.ByteString.Lazy (ByteString)
import Data.Proxy
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy (pack)
import Data.Aeson

import Control.Monad.Trans.Except
import System.IO
import GHC.Generics

import Network.HTTP.Types
import Network.Wai.Handler.Warp
import Network.Wai
import Servant
import Servant.Server
import Servant.Docs

import SimpleFormulaChecker (compareSpec)
import qualified SimpleFormulaChecker as S
import LogicIR.Backend.Z3.Model

-- | Data types.
type ApiArg = String
data ApiResponseType = Equivalent | NotEquivalent | Undefined
  deriving Generic
data ApiResponse = ApiResponse
  { responseType :: ApiResponseType
  , model :: Maybe Z3Model
  }
  deriving Generic
instance ToJSON ApiResponseType
instance ToJSON ApiResponse
instance ToJSON ModelVal where
  toJSON (BoolVal b) = toJSON b
  toJSON (IntVal n) = toJSON n
  toJSON (RealVal n) = toJSON n
  toJSON (ManyVal vs) = Array $ fromList $ map toJSON vs

type CompareApi = "compare"
  :> QueryParam "a" ApiArg
  :> QueryParam "b" ApiArg
  :> Get '[JSON] ApiResponse

compareApi :: Proxy CompareApi
compareApi = Proxy

-- | API documentation.
instance ToParam (QueryParam "a" String) where
  toParam _ =
    DocQueryParam "a" ["examples/javawlp_edsl/src/nl/uu/javawlp_edsl/Main.java:real1", "..."]
                  "Method location formatted as: <java_source_file>:<method_name>"
                  Normal

instance ToParam (QueryParam "b" String) where
  toParam _ =
    DocQueryParam "b" ["examples/javawlp_edsl/src/nl/uu/javawlp_edsl/Main.java:real2", "..."]
                  "Method location formatted as: <java_source_file>:<method_name>" Normal

instance ToSample ApiResponse where
  toSamples _ = singleSample ApiResponse
    { responseType = NotEquivalent
    , model = Just $ M.fromList
      [ ("a", ManyVal [RealVal 0.0, RealVal (-0.5)])
      , ("i", IntVal 10)
      ]
    }

docsBS :: ByteString
docsBS = encodeUtf8
       . pack
       . markdown
       $ docsWithIntros [intro] compareApi
  where intro = DocIntro "Welcome" ["This is our webservice's API.", "Enjoy!"]

apiDocs :: API
apiDocs = docs compareApi

type DocsApi = CompareApi :<|> Raw

docsApi :: Proxy DocsApi
docsApi = Proxy

server :: Server DocsApi
server = server' :<|> Tagged serveDocs where
    serveDocs _ respond =
        respond $ responseLBS ok200 [plain] docsBS
    plain = ("Content-Type", "text/plain")

app :: Application
app = serve docsApi server

-- | Server implementation.
runApi :: IO ()
runApi = do
  let port = 8888
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
        defaultSettings
  runSettings settings app

server' :: Server CompareApi
server' = getCompareResponse

getCompareResponse :: Maybe String -> Maybe String -> Handler ApiResponse
getCompareResponse a b = do
    let [srcA, methodA] = splitOn ":" (fromJust a)
    let [srcB, methodB] = splitOn ":" (fromJust b)
    response <- liftIO $ compareSpec (srcA, methodA) (srcB, methodB)
    return $ case response of
                  S.Equivalent ->
                    ApiResponse { responseType = Equivalent, model = Nothing }
                  S.NotEquivalent model ->
                    ApiResponse { responseType = NotEquivalent, model = Just model }
                  _ -> ApiResponse { responseType = Undefined, model = Nothing }
