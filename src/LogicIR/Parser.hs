{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module LogicIR.Parser where

import Data.String
import Text.Parsec hiding (runP)
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as Tokens

import LogicIR.ParserUtils
import LogicIR.Expr

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
                , [ infix_ "<==>" (.<==>) ]
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
  x <- exprP <~ "==" <~ "null"
  return $ LIsnull x
lenP = do
  x <- "len(" ~> exprP <~ ")"
  return $ LLen x
arrayP = do
  x <- exprP <~ "["
  e <- exprP <~ "]"
  return $ x .! e
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
                   <|> (PInt <$ str "int")
                   <|> (PReal <$ str "real")
    arrayTypeP = "[" ~> typeP <~ "]"

-- | Implicit conversions from strings.
instance IsString LExpr where
  fromString = runP exprP
instance IsString Var where
  fromString = runP bvarP
instance IsString Type where
  fromString = runP typeP
