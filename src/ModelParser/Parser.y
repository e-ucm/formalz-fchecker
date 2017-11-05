{
module ModelParser.Parser (parseModel) where

import ModelParser.Lexer
import ModelParser.Model
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

parseError :: [Token] -> a
parseError t = error $ "Model parse error " ++ show t

parseModel :: String -> Z3Model
parseModel = happyParseTokens . alexScanTokens

}