{
module ModelParser.Parser where

import ModelParser.Lexer
}

%name happyParseTokens
%tokentype { Token }
%error { parseError }
%token
    "->"       { Tarrow }
    "{"        { Tbopen }
    "}"        { Tbclose }
    "("        { Tpopen }
    ")"        { Tpclose }
    "_"        { Tunderscore }
    "else"     { Telse }
    "as-array" { Tasarray }
    "BitVec"   { Tbitvec }
    "Array"    { Tarray }
    "as"       { Tas }
    "const"    { Tconst }
    value      { Tvalue $$ }
    bool       { Tbool $$ }
    ident      { Tident $$ }
%%

Model : ModelList { $1 }

ModelList : ModelEntry ModelList { $1 : $2 }
          | {- empty -} { [] }

ModelEntry : ident "->" ModelVal { ($1, $3) }

ModelVal : bool { BoolVal $1 }
         | value { IntVal $1 }
         | "(" "_" "as-array" ident ")" { ArrayRef $4 }
         | "(" "(" "as" "const" "(" "Array" "(" "_" "BitVec" value ")" "(" "_" "BitVec" value ")" ")" ")" value ")" { ArrayAsConst $19 }
         | "{" FuncList "}" { ArrayFunc $2 }

FuncList : FuncEntry FuncList { $1 : $2 }
         | {- empty -} { [] }

FuncEntry : value "->" value { InstInt $1 $3 }
          | "else" "->" value { InstElse $3 }
          | value { InstElse $1 }

{
data FuncInst = InstInt Int Int
              | InstElse Int
              deriving (Show, Read, Eq)

data ModelVal = BoolVal Bool
              | IntVal Int
              | ArrayRef String -- TODO: immediately forward to ArrayFunc?
              | ArrayAsConst Int -- TODO: parse to InstElse?
              | ArrayFunc [FuncInst]
              deriving (Show, Read, Eq)

type Model = [(String, ModelVal)]

parseError :: [Token] -> a
parseError t = error $ "Parse error" ++ show t

parseModel :: String -> Model
parseModel = happyParseTokens . alexScanTokens
}