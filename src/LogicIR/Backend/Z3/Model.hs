{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module LogicIR.Backend.Z3.Model where

import Data.String
import System.IO.Unsafe (unsafePerformIO)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as Tokens

data FuncInst = InstVal Integer ModelVal
              | InstElse ModelVal
              deriving (Show, Read, Eq, Ord)

data ModelVal = BoolVal Bool
              | IntVal Integer
              | RealVal Double
              | ArrayRef String
              | ArrayFunc [FuncInst]
              deriving (Read, Eq, Ord)

type Z3Model = [(String, ModelVal)]

instance Show ModelVal where
  show (BoolVal b) = show b
  show (IntVal i) = show i
  show (RealVal i) = show i
  show (ArrayRef s) = s
  show (ArrayFunc fs) = show fs

-- | Parser.
runP :: Parser a -> String -> a
runP p s = unsafePerformIO $
  case runParser (p <* eof) () "" s of
        Left err   -> fail $ show err
        Right lExp -> return lExp

runP' :: Parser a -> String -> Either ParseError a
runP' p = runParser (p <* eof) () ""

modelP :: Parser Z3Model
modelP = many1 (lexeme asgP)

asgP :: Parser (String, ModelVal)
asgP = do
  identifier <- idP <~ "->"
  val <- valP
  return (identifier, val)

valP :: Parser ModelVal
valP = try (parens valP)
   <|> try (ArrayFunc <$> funcP)
   <|> try (ArrayRef <$> arrP)
   <|> try (RealVal <$> realP)
   <|> try (IntVal <$> intP)
   <|> (BoolVal <$> boolP)

funcP :: Parser [FuncInst]
funcP = "{" ~> many1 funcInstP <~ "}"

arrP :: Parser String
arrP = "(_ as-array" ~> idP <~ ")"

funcInstP :: Parser FuncInst
funcInstP = try instElse
        <|> try instVal
        <|> instConst
  where instVal = do
          i <- (indexP <~ "->") <?> "index"
          v <- valP <?> "val"
          return $ InstVal i v
        instElse = do
          v <- "else ->" ~> valP
          return $ InstElse v
        instConst = InstElse <$> valP

idP :: Parser String
idP = many1 (letter <|> digit <|> oneOf ['!', '?'])

realP :: Parser Double
realP = negRealP <|> Tokens.float haskell
  where negRealP = do v <- "-" ~> Tokens.float haskell
                      return (-v)

intP :: Parser Integer
intP = Tokens.integer haskell

indexP :: Parser Integer
indexP = Tokens.natural haskell

boolP :: Parser Bool
boolP = (True <$ str "true") <|> (False <$ str "false")

-- | Useful marcros.
lexeme p = spaces *> p <* spaces
str = lexeme . string
commas l = l `sepBy` string ", "
(<~) p s = p <* lexeme (string s)
(~>) s p = lexeme (string s) *> p
parens = Tokens.parens haskell
reserved = Tokens.reservedOp haskell
infix_ op f = Infix (reserved op >> return f) AssocLeft
prefix op f = Prefix (reserved op >> return f)
postfix op f = Postfix (reserved op >> return f)
