{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Server (runApi) where

import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Proxy
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Vector (fromList)

import GHC.Generics
import System.IO

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Docs

import Data.Swagger hiding (port)
import Servant.Swagger
import Servant.Swagger.UI

import API (Mode (..), ParseMode (..), compareSpec)
import Model

-- | Data types.
data ApiReqBody = ApiReqBody
  { sourceA :: String
  , sourceB :: String
  } deriving (Eq, Show, Generic)
instance FromJSON ApiReqBody
instance ToJSON ApiReqBody

data ApiResponseType = Equiv | NotEquiv | Undef | ResponseErr
  deriving (Eq, Show, Generic)

data ApiResponse = ApiResponse
  { responseType :: ApiResponseType
  , model        :: Maybe Model
  , err          :: Maybe String
  }
  deriving (Eq, Show, Generic)
instance ToJSON ApiResponseType
instance ToJSON ApiResponse
instance ToJSON ModelVal where
  toJSON (BoolVal b)  = toJSON b
  toJSON (IntVal n)   = toJSON n
  toJSON (RealVal n)  = toJSON n
  toJSON (ManyVal vs) = Array $ fromList $ map toJSON vs

type CompareApi = "compare"
  :> ReqBody '[JSON] ApiReqBody
  :> Post '[JSON] ApiResponse

serverCompare :: Server CompareApi
serverCompare = getCompareResponse

getCompareResponse :: ApiReqBody -> Handler ApiResponse
getCompareResponse ApiReqBody {sourceA = srcA, sourceB = srcB} = do
    resp <- liftIO $ compareSpec Release Raw (wrap srcA) (wrap srcB)
    return $ case resp of
                  Equivalent ->
                    ApiResponse { responseType = Equiv, model = Nothing, err = Nothing }
                  NotEquivalent m ->
                    ApiResponse { responseType = NotEquiv, model = Just m, err = Nothing }
                  ErrorResponse e ->
                    ApiResponse { responseType = ResponseErr, model = Nothing, err = Just e }
                  _ ->
                    ApiResponse { responseType = Undef, model = Nothing, err = Nothing }
    where
      wrap s = ( "public class Main {" ++ s ++ "}"
               , last $ splitOn " " $ head $ splitOn "(" s
               )

-- | API documentation (Markdown).
instance ToSample ApiReqBody where
  toSamples _ = singleSample $ ApiReqBody srcA srcB
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
    , err = Nothing
    }

docsBS :: ByteString
docsBS = encodeUtf8
       . pack
       . markdown
       $ docsWith opts [intro] mempty (Proxy :: Proxy CompareApi)
  where intro = DocIntro "Welcome" ["This is our webservice's API.", "Enjoy!"]
        opts = DocOptions 1

-- | API documentation (Swagger).
instance ToSchema ApiReqBody
instance ToSchema ApiResponseType
instance ToSchema ApiResponse
instance ToSchema ModelVal where
  declareNamedSchema _ = return $ NamedSchema Nothing $
    sketchSchema (ManyVal [BoolVal False, IntVal 23, RealVal 7.5])

type SwagApi = SwaggerSchemaUI "api-swagger" "swagger.json"

serverSwagger :: Server SwagApi
serverSwagger = swaggerSchemaUIServer (toSwagger (Proxy :: Proxy CompareApi))

-- | Server.
runApi :: Int -> IO ()
runApi port = runSettings settings app
  where settings = ( setPort port
                   . setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
                   ) defaultSettings

type WholeApi = CompareApi :<|> SwagApi :<|> Raw

app :: Application
app = serve (Proxy :: Proxy WholeApi) $
        serverCompare :<|> serverSwagger :<|> Tagged serveDocs
      where serveDocs _ respond = respond $
              responseLBS ok200 [("Content-Type", "text/plain")] docsBS
