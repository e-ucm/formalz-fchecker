{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Server (runApi) where

import Data.Maybe
import qualified Data.Map as Map
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

import API (compareSpec, Mode (..), ParseMode (..))
import Model

-- | Data types.
data ApiReqBody = ApiReqBody
  { sourceA :: String
  , sourceB :: String
  } deriving Generic
instance FromJSON ApiReqBody
instance ToJSON ApiReqBody

data ApiResponseType = Equiv | NotEquiv | Undef
  deriving Generic
data ApiResponse = ApiResponse
  { responseType :: ApiResponseType
  , model :: Maybe Model
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
  :> ReqBody '[JSON] ApiReqBody
  :> Post '[JSON] ApiResponse

compareApi :: Proxy CompareApi
compareApi = Proxy

-- | API documentation.
instance ToSample ApiReqBody where
  toSamples _ = singleSample $ ApiReqBody srcA srcA
    where srcA = "public static float real1(float a) {\
                  \ pre(a >= (2 - 1 + 1));\
                  \ a += a;\
                  \ post(a >= (4 - 3 + 3));}"
          srcB = "public static float real2(float a) {\
                  \ pre(a > 2 || a == 2);\
                  \ a = a * 2;\
                  \ post(a > 4 || a == 4);}"

instance ToSample ApiResponse where
  toSamples _ = singleSample ApiResponse
    { responseType = NotEquiv
    , model = Just $ Map.fromList
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

getCompareResponse :: ApiReqBody -> Handler ApiResponse
getCompareResponse (ApiReqBody srcA srcB) = do
    response <- liftIO $ compareSpec Release Raw (wrap srcA) (wrap srcB)
    return $ case response of
                  Equivalent ->
                    ApiResponse { responseType = Equiv, model = Nothing }
                  NotEquivalent model ->
                    ApiResponse { responseType = NotEquiv, model = Just model }
                  _ -> ApiResponse { responseType = Undef, model = Nothing }
    where
      wrap s = ( "public class Main {" ++ s ++ "}"
               , last $ splitOn " " $ head $ splitOn "(" s
               )
