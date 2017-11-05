{
module ModelParser.Lexer where

import Numeric
}
 
%wrapper "basic"
 
$ident = [0-9a-zA-Z!\._\?]
$num = [0-9]
$value = [0-9a-f]
 
tokens :-
    $white+     ;
    "->"        { \s -> Tarrow }
    "{"         { \s -> Tbopen }
    "}"         { \s -> Tbclose }
    "("         { \s -> Tpopen }
    ")"         { \s -> Tpclose }
    "_"         { \s -> Tunderscore }
    "else"      { \s -> Telse}
    "as-array"  { \s -> Tasarray }
    "BitVec"    { \s -> Tbitvec }
    "Array"     { \s -> Tarray }
    "as"        { \s -> Tas }
    "const"     { \s -> Tconst }
    "true"      { \s -> Tbool True }
    "false"     { \s -> Tbool False }
    "#x"$value+ { \s -> Tvalue (parseHex s) }
    $num+       { \s -> Tvalue (parseDec s) }
    $ident+     { \s -> Tident s }    
{

data Token = Tarrow
           | Tbopen
           | Tbclose
           | Tpopen
           | Tpclose
           | Tunderscore
           | Telse
           | Tasarray
           | Tbitvec
           | Tarray
           | Tas
           | Tconst
           | Tvalue Int
           | Tbool Bool
           | Tident String
     deriving (Eq, Show)

parseHex :: String -> Int
parseHex ('#' : 'x' : value) = let [(n, "")] = readHex value in n

parseDec :: String -> Int
parseDec value = let [(n, "")] = readDec value in n

model1 = "a.null -> false\nj -> #x00000000\ni -> #x00000000\na.length -> #x00000001"
model1a = model1 ++ "\na -> (_ as-array k!1)"
model1b = model1 ++ "\na -> ((as const (Array (_ BitVec 32) (_ BitVec 32))) #xffffffff)"
model2 = "a.length -> #x00000001\na.null -> true\ni!0 -> #x00000000\na -> (_ as-array k!1)\nk!1 -> {\n  #x00000000\n}"
model3 = "a.length -> #x00000003\ni!0 -> #x00000001\na -> (_ as-array k!1)\nk!1 -> {\n  #x00000002 -> #x00000001\n  #x00000001 -> #x00000000\n  else -> #x00000001\n}"
model4 = "a.null -> false\nj -> #x00000010\na -> ((as const (Array (_ BitVec 32) (_ BitVec 32))) #xffffffff)\ni -> #x00000000\na.length -> #x00000001"

}