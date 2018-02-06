module SimpleFormulaChecker where

import Control.Exception.Base (catch)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import qualified Data.Map as M
import Data.Maybe

import Javawlp.Engine.HelperFunctions
import Javawlp.Engine.Types
import Language.Java.Parser
import Language.Java.Pretty
import Language.Java.Syntax

import qualified LogicIR.Backend.Z3.API as Z3
import LogicIR.Expr
import LogicIR.Eval
import LogicIR.Backend.Z3.Z3
import LogicIR.Backend.Z3.Model
import LogicIR.Backend.QuickCheck.Test

import LogicIR.Frontend.Java (javaExpToLExpr)
import LogicIR.Null (lExprPreprocessNull)
import LogicIR.Pretty (prettyLExpr)

-- | Response type.
data Response = Equivalent | NotEquivalent Z3Model | Undefined | Timeout
                deriving (Eq, Show)

(<>) :: Response -> Response -> Response
Equivalent <> r = r
NotEquivalent s <> _ = NotEquivalent s
Timeout <> _ = Timeout
Undefined <> _ = Undefined

-- Function that compares both the pre and the post condition for two methods.
-- It is assumed that both methods have the same environment (parameter names, class member names, etc).
compareSpec :: (FilePath, String) -> (FilePath, String) -> IO Response
compareSpec method1@(_, name1) method2@(_, name2) = do
    -- load the methods
    m1@(decls1, mbody1, env1) <- parseMethod method1
    m2@(decls2, mbody2, env2) <- parseMethod method2
    when (decls1 /= decls2) $ fail "inconsistent class declarations"
    -- when (env1 /= env2) $ fail "inconsistent environments"
    putStrLn $ "----PRE---- (" ++ name1 ++ " vs " ++ name2 ++ ")"
    preAns <- determineFormulaEq m1 m2 "pre"
    putStrLn "\n----POST---"
    postAns <- determineFormulaEq m1 m2 "post"
    return $ preAns <> postAns

-- Determine the equality of two method's pre/post conditions.
determineFormulaEq :: MethodDef -> MethodDef -> String -> IO Response
determineFormulaEq m1@(decls1, mbody1, env1) m2@(decls2, mbody2, env2) name = do
    -- get pre/post condition
    let (e1, e2) = (extractCond m1 name, extractCond m2 name)
    putStrLn $ "e1:\n" ++ prettyPrint e1 ++ "\n\ne2:\n" ++ prettyPrint e2 ++ "\n"
    let (l1, l2) = (javaExpToLExpr e1 env1 decls1, javaExpToLExpr e2 env2 decls2)
    let (l, l') = (lExprPreprocessNull l1, lExprPreprocessNull l2) -- preprocess "a == null" to "isNull(a)"
    putStrLn $ "LogicIR.Pretty 1:\n" ++ prettyLExpr l ++ "\n\nLogicIR.Pretty 2:\n" ++ prettyLExpr l' ++ "\n"
    z3Response <- l `Z3.equivalentTo` l'
    case z3Response of
      Z3.Equivalent      -> return Equivalent
      Z3.NotEquivalent s -> return $ NotEquivalent s
      Z3.Timeout         -> return Timeout -- TODO add QuickCheck on timeout
      _                  -> return Undefined
    where
      extractCond :: MethodDef -> String -> Exp
      extractCond m n = extractExpr (getMethodCalls m n)

type MethodDef = ([TypeDecl], Stmt, TypeEnv)

-- Takes a Java source file and a method name and returns the class declarations,
-- Returns the method body and the method's formal parameters.
parseMethod :: (FilePath, String) -> IO MethodDef
parseMethod (src, name) = do
    decls <- parseDecls src
    -- get the method's body (assuming all methods have different names)
    let mbody = fromJust $ getMethod decls (Ident name)
    -- get the method's formal parameters:
    let env = getMethodTypeEnv decls (Ident name)
    -- return the relevant data
    return (decls, mbody, env)

-- Get a list of all calls to a method of a specific name from a method definition.
getMethodCalls :: MethodDef -> String -> [MethodInvocation]
getMethodCalls (_, StmtBlock (Block bs), _) name = mapMaybe extractMethodInv bs
    where
        extractMethodInv :: BlockStmt -> Maybe MethodInvocation
        extractMethodInv (BlockStmt (ExpStmt (MethodInv i@(MethodCall (Name [Ident n]) _)))) = if n == name then Just i else Nothing
        extractMethodInv _ = Nothing

-- [pre(a), pre(b), pre(c)] -> (a AND b AND c)
extractExpr :: [MethodInvocation] -> Exp
extractExpr call = combineExprs $ map (\(MethodCall (Name [Ident _]) [a]) -> a) call
    where combineExprs :: [Exp] -> Exp
          combineExprs []     = true
          combineExprs [e]    = e
          combineExprs (e:es) = e &* combineExprs es

parse :: (FilePath, String) -> (FilePath, String) -> IO (MethodDef, MethodDef)
parse method1@(_, name1) method2@(_, name2) = do
    -- load the methods
    m1@(decls1, mbody1, env1) <- parseMethod method1
    m2@(decls2, mbody2, env2) <- parseMethod method2
    when (env1 /= env2) $ fail "inconsistent method parameters"
    when (decls1 /= decls2) $ fail "inconsistent class declarations (TODO)"
    return (m1, m2)

methodDefToLExpr :: MethodDef -> MethodDef -> String -> (LExpr, LExpr)
methodDefToLExpr m1@(decls1, _, env1) m2@(decls2, _, env2) name = do
    -- get pre/post condition
    let (e1, e2) = (extractCond m1 name, extractCond m2 name)
    let (lExpr1', lExpr2') = (javaExpToLExpr e1 env1 decls1, javaExpToLExpr e2 env2 decls2)
    -- preprocess "a == null" to "isNull(a)"
    let (lExpr1, lExpr2) = (lExprPreprocessNull lExpr1', lExprPreprocessNull lExpr2')
    (lExpr1, lExpr2)
        where extractCond m n = extractExpr $ getMethodCalls m n

testSpec :: (FilePath, String) -> (FilePath, String) -> Int -> IO Bool
testSpec method1@(_, name1) method2@(_, name2) n = do
    (m1, m2) <- parse method1 method2
    putStrLn $ "----PRE---- (" ++ name1 ++ " vs " ++ name2 ++ ")"
    let (lExpr1, lExpr2) = methodDefToLExpr m1 m2 "pre"
    (preAns, counterModel) <- testEquality n lExpr1 lExpr2
    putStrLn "\n----POST---"
    let (lExpr1, lExpr2) = methodDefToLExpr m1 m2 "post"
    (postAns, counterModel) <- testEquality n lExpr1 lExpr2
    return $ preAns && postAns
