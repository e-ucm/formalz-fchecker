{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Server
  ( app, compareApp, runApi       -- spawn server
  , compareApi, swagApi, wholeApi -- API proxies
  , ApiReqBody (..), ApiResponse (..), ApiResponseType (..)
  ) where

import Prelude hiding (log)
import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.Aeson.Types hiding (Options)
import Data.ByteString.Lazy (ByteString)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Proxy
import Data.Scientific
import Data.Text.Lazy          (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Vector as Vec

import GHC.Generics
import System.IO

import Network.HTTP.Types
import Network.Wai hiding (Response)
import Network.Wai.Handler.Warp
import Servant
import Servant.Docs hiding (Response)

import Data.Swagger hiding (port, Response)
import Servant.Swagger
import Servant.Swagger.UI

import API
import Model

-- | Data types.
data ApiReqBody = ApiReqBody
  { sourceA :: String
  , sourceB :: String
  , options :: Options
  } deriving (Eq, Generic)
instance FromJSON Mode
instance ToJSON   Mode
instance FromJSON ParseMode
instance ToJSON   ParseMode
instance FromJSON Options where
  parseJSON (Object o) = Options <$> o .:? "mode"      .!= md
                                 <*> o .:? "parseMode" .!= pmd
                                 <*> o .:? "checkPre"  .!= cpr
                                 <*> o .:? "checkPost" .!= cpo
                           where (Options md pmd cpr cpo) = defOptions
  parseJSON invalid    = typeMismatch "Options" invalid
instance ToJSON   Options
instance FromJSON ApiReqBody where
  parseJSON (Object o) = ApiReqBody <$> o .: "sourceA"
                                    <*> o .: "sourceB"
                                    <*> o .:? "options" .!= defOptions
  parseJSON invalid    = typeMismatch "ApiReqBody" invalid
instance ToJSON   ApiReqBody
instance Show ApiReqBody where
  show (ApiReqBody srcA srcB opts) =
    "srcA: " ++ srcA ++ "\nsrcB: " ++ srcB ++ "\nopts: " ++ show opts

data ApiResponseType = Equiv | NotEquiv | Undef | ResponseErr
  deriving (Eq, Show, Generic)

data ApiResponse = ApiResponse
  { responseType :: ApiResponseType
  , model        :: Maybe Model
  , err          :: Maybe String
  , feedback     :: Maybe Feedback
  }
  deriving (Eq, Generic)
instance FromJSON Feedback
instance ToJSON   Feedback

instance ToJSON   ApiResponseType
instance FromJSON ApiResponseType

instance ToJSON   ApiResponse
instance FromJSON ApiResponse

instance FromJSON ModelVal where
  parseJSON (Bool b) = return $ BoolVal b
  parseJSON (Number n) = return $
    case floatingOrInteger n of
      Left f  -> RealVal f
      Right i -> IntVal i
  parseJSON (Array a) = (ManyVal . Vec.toList) <$> Vec.sequence (parseJSON <$> a)
  parseJSON invalid = typeMismatch "ModelVal" invalid
instance ToJSON ModelVal where
  toJSON (BoolVal b)  = toJSON b
  toJSON (IntVal n)   = toJSON n
  toJSON (RealVal n)  = toJSON n
  toJSON (ManyVal vs) = Array $ Vec.fromList $ map toJSON vs
instance Show ApiResponse where
  show (ApiResponse rType mdl e fd) =
    "type: " ++ show rType ++ "\nmodel: " ++ show mdl ++ "\nerror: "
      ++ show e ++ "\nfeedback: " ++ show fd

-- | Default API response.
defResp :: ApiResponse
defResp = ApiResponse { responseType = Undef
                      , model = Nothing
                      , err = Nothing
                      , feedback = Nothing
                      }

type CompareApi = "compare"
  :> ReqBody '[JSON] ApiReqBody
  :> Post '[JSON] ApiResponse

compareApi :: Proxy CompareApi
compareApi = Proxy

serverCompare :: Server CompareApi
serverCompare = getCompareResponse

getCompareResponse :: ApiReqBody -> Handler ApiResponse
getCompareResponse req@(ApiReqBody srcA srcB opts) = do
    liftIO $ log $
      "\n******************************************************************\n" ++
      show req ++
      "\n------------------------------------------------------------------\n"
    resp <- toResp <$> liftIO (compareSpec opts (wrap srcA) (wrap srcB))
    liftIO $ log $
      "\n------------------------------------------------------------------\n" ++
      show resp ++
      "\n******************************************************************\n"
    return resp
    where
      toResp :: Response -> ApiResponse
      toResp (NotEquivalent mdl f)
        = defResp { responseType = NotEquiv
                  , model = Just mdl, feedback = Just f }
      toResp (Equivalent _)
        = defResp { responseType = Equiv }
      toResp (ErrorResponse e)
        = defResp { responseType = ResponseErr, err = Just e }
      toResp _
        = defResp { responseType = Undef }

      wrap s = ( "public class Main {" ++ s ++ "}"
               , last $ splitOn " " $ head $ splitOn "(" s
               )

-- | API documentation (Markdown).
instance ToSample ApiReqBody where
  toSamples _ = singleSample $ ApiReqBody srcA srcB defOptions
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
    , feedback = Just $ Feedback (True, True, False, True) (False, True, True, False)
    }

docsBS :: ByteString
docsBS = encodeUtf8
       . pack
       . markdown
       $ docsWith opts [intro] mempty compareApi
  where intro = DocIntro "Welcome" ["This is our webservice's API.", "Enjoy!"]
        opts = DocOptions 1

-- | API documentation (Swagger).
instance ToSchema Feedback
instance ToSchema Mode
instance ToSchema ParseMode
instance ToSchema Options
instance ToSchema ApiReqBody
instance ToSchema ApiResponseType
instance ToSchema ApiResponse
instance ToSchema ModelVal where
  declareNamedSchema _ = return $ NamedSchema Nothing $
    sketchSchema (ManyVal [BoolVal False, IntVal 23, RealVal 7.5])

type SwagApi = SwaggerSchemaUI "api-swagger" "swagger.json"

swagApi :: Proxy SwagApi
swagApi = Proxy

serverSwagger :: Server SwagApi
serverSwagger = swaggerSchemaUIServer (toSwagger compareApi)

-- | Server.
runApi :: Int -> IO ()
runApi port = runSettings settings app
  where settings = ( setPort port
                   . setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
                   ) defaultSettings

type WholeApi = CompareApi :<|> SwagApi :<|> Raw

wholeApi :: Proxy WholeApi
wholeApi = Proxy

compareApp :: Application
compareApp = serve compareApi serverCompare

app :: Application
app = serve wholeApi $
        serverCompare :<|> serverSwagger :<|> Tagged serveDocs
      where serveDocs _ respond = respond $
              responseLBS ok200 [("Content-Type", "text/plain")] docsBS
