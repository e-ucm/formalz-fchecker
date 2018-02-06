{-# LANGUAGE FlexibleContexts  #-}
module LogicIR.ParserUtils where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as Tokens

-- | Run parser.
runP :: Parser a -> String -> a
runP p s =
  case runParser (p <* eof) () "" s of
        Left err   -> error $ show err
        Right x -> x

-- | Useful marcros.
lexeme p = spaces *> p <* spaces
str = lexeme . string :: String -> Parser String
commas l = l `sepBy` string ", "
(<~) p s = p <* lexeme (string s)
(~>) s p = lexeme (string s) *> p
parens = Tokens.parens haskell
identifier = Tokens.identifier haskell
reserved = Tokens.reservedOp haskell
infix_ op f = Infix (reserved op >> return f) AssocLeft
prefix op f = Prefix (reserved op >> return f)
postfix op f = Postfix (reserved op >> return f)
