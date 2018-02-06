{-# LANGUAGE OverloadedStrings #-}
module LogicIR.Backend.Z3.Model where

import qualified Data.Map as M
import Data.String
import Text.Parsec hiding (runP)
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as Tokens

import LogicIR.ParserUtils

-- | Data type.
data ModelVal = BoolVal Bool
              | IntVal Integer
              | RealVal Double
              | ManyVal [ModelVal]
              deriving (Eq)

type Z3Model = M.Map String ModelVal
emptyZ3Model = M.empty :: Z3Model

-- | Pretty-printing.
instance Show ModelVal where
  show (BoolVal b)  = show b
  show (IntVal i)   = show i
  show (RealVal i)  = show i
  show (ManyVal vs) = show vs

-- | Crop arrays until their specified length.
sanitize :: Z3Model -> Z3Model
sanitize model = M.mapWithKey f model
  where f k (ManyVal array) = ManyVal $ take (counts k) array
        f _ x = x
        counts k = fromInteger $
          case model M.! (k ++ "?length") of
            (IntVal i) -> i
            _ -> error "non-int length"

-- | Parsers.
modelP :: Parser ModelVal
modelP = try (BoolVal <$> boolP)
     <|> try (RealVal <$> realP)
     <|> (IntVal <$> intP)

realP :: Parser Double
realP =  try negRealP <|> try divRealP <|> Tokens.float haskell
  where divRealP = do
          v <- "(/" ~> realP
          v' <- realP <~ ")"
          return $ v / v'
        negRealP = do
          v <- "(-" ~> realP <~ ")"
          return $ -v

intP :: Parser Integer
intP = try negIntP <|> Tokens.integer haskell
  where negIntP = do v <- "(-" ~> intP <~ ")"
                     return $ -v

indexP :: Parser Integer
indexP = Tokens.natural haskell

boolP :: Parser Bool
boolP = (True <$ str "true") <|> (False <$ str "false")

-- | Implicit conversion from strings.
instance IsString ModelVal where
  fromString = runP modelP
