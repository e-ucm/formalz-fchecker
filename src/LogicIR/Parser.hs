{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module LogicIR.Parser where

import Data.String
import System.IO.Unsafe (unsafePerformIO)
import Text.Parsec hiding (runP)
import Text.Parsec.Char
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as Tokens

import LogicIR.Expr

-- | Run parser.
runP :: Parser a -> String -> a
runP p s = unsafePerformIO $
  case runParser (p <* eof) () "" s of
        Left err   -> fail $ show err
        Right lExp -> return lExp

-- | Parser for logical expressions.
exprP :: Parser LExpr
exprP =
  buildExpressionParser table term <?> "expression"
  where table = [ -- Numeric
                  [ infix_ "*" (.*), infix_ "/" (./), infix_ "%" (.%) ]
                , [ infix_ "+" (.+) , infix_ "-" (.-) ]
                  -- Comparison
                , [ infix_ "==" (.==) , infix_ "!=" (.!=) , infix_ "<" (.<)
                  , infix_ "<=" (.<=) , infix_ ">" (.>) , infix_ ">=" (.>=)
                  ]
                  -- Logical
                , [ prefix "!" lnot]
                , [ infix_ "/\\" (.&&), infix_ "\\/" (.||) ]
                , [ infix_ "==>" (.==>) ]
                ]
        term = try (parens exprP) <|>
               try nullP <|>
               try arrayP <|>
               try lenP <|>
               try (parens quantP) <|>
               try (parens iteP) <|>
               try varP <|>
               constP

-- | Sub-parsers.
nullP, lenP, arrayP, quantP, iteP, varP, constP :: Parser LExpr
nullP = do
  v <- bvarP <~ "==" <~ "null"
  return $ LIsnull v
lenP = do
  v <- "len(" ~> bvarP <~ ")"
  return $ LLen v
arrayP = do
  v <- bvarP <~ "["
  e <- exprP <~ "]"
  return $ v .! e
quantP = try forallP <|> existsP
  where forallP = do
          bvar <- "forall " ~> bvarP <~ "::"
          domain <- exprP <~ "->"
          e <- exprP
          return $ forall bvar domain e
        existsP = do
          bvar <- "exists " ~> bvarP <~ "::"
          domain <- exprP <~ "->"
          e <- exprP
          return $ exists bvar domain e
iteP = do
  g <- exprP <~ "?"
  e <- exprP <~ ":"
  e' <- exprP
  return $ LIf g e e'
varP = LVar <$> bvarP
constP = LConst <$> ((CBool <$> boolP)
                 <|> (CInt <$> numP)
                 <|> (CReal <$> realP)
                 <|> (CNil <$ nilP)
                 )
  where
    boolP = (True <$ str "true") <|> (False <$ str "false")
    numP = lexeme $ fmap fromInteger (Tokens.integer haskell)
    realP = lexeme $ Tokens.float haskell
    nilP = str "null"

bvarP :: Parser Var
bvarP = do
  id_ <- identifier
  type_ <- ":" ~> typeP
  return $ Var type_ id_

typeP :: Parser Type
typeP = try (TPrim <$> primTypeP) <|> (TArray <$> arrayTypeP)
  where
    primTypeP = lexeme (PBool <$ str "bool")
                   <|> (PInt32 <$ str "int")
                   <|> (PReal <$ str "real")
    arrayTypeP = "[" ~> typeP <~ "]"

-- | Useful marcros.
lexeme p = spaces *> p <* spaces
str = lexeme . string
commas l = l `sepBy` string ", "
(<~) p s = p <* lexeme (string s)
(~>) s p = lexeme (string s) *> p
parens = Tokens.parens haskell
identifier = Tokens.identifier haskell
reserved = Tokens.reservedOp haskell
infix_ op f = Infix (reserved op >> return f) AssocLeft
prefix op f = Prefix (reserved op >> return f)
postfix op f = Postfix (reserved op >> return f)

-- | Implicit conversions from strings.
instance IsString LExpr where
  fromString = runP exprP
instance IsString Var where
  fromString = runP bvarP
instance IsString Type where
  fromString = runP typeP
