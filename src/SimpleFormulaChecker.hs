module SimpleFormulaChecker where

import Control.Exception.Base (catch)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Control.Concurrent
import qualified Data.Map as M
import Data.Maybe
import Data.String

import Javawlp.Engine.HelperFunctions
import Javawlp.Engine.Types
import Language.Java.Parser
import Language.Java.Pretty
import Language.Java.Syntax

import qualified LogicIR.Backend.Z3.API as Z3
import qualified LogicIR.Backend.QuickCheck.API as Test
import LogicIR.Expr
import LogicIR.Eval
import LogicIR.Backend.Z3.Z3
import LogicIR.Backend.Z3.Model

import LogicIR.Frontend.Java (javaExpToLExpr)
import LogicIR.Null (lExprPreprocessNull)
import LogicIR.Pretty (prettyLExpr)

-- | Response type.
data Response = Equivalent | NotEquivalent Z3Model | Undefined | Timeout
                deriving (Eq)

(<>) :: Response -> Response -> Response
Equivalent <> r = r
NotEquivalent s <> _ = NotEquivalent s
Timeout <> _ = Timeout
Undefined <> _ = Undefined

instance Show Response where
  show Equivalent = "Formulas are equivalent"
  show Undefined = "Oops... could not determine if formulas are equivalent"
  show Timeout = "Timeout occured"
  show (NotEquivalent model) = "Not equivalent: " ++ show model

-- | Mode type.

data Mode = Debug | Release
            deriving (Eq)

instance Show Mode where
    show Debug   = "Debug"
    show Release = "Release"

-- | Calls proveSpec and testSpec on different threads and returns
--   their response. If the function is called in debug mode, it
--   compares the result of testSpec and proveSpec. Otherwise, it 
--   returns the answer of the fastest method of the two.
compareSpec :: Mode               -- ^ The execution mode
            -> (FilePath, String) -- ^ Specification 1
            -> (FilePath, String) -- ^ Specification 2
            -> IO Response        -- ^ Result of spec comparison.
compareSpec m m1 m2 = do
    mv1 <- newEmptyMVar
    mv2 <- if m == Debug then newEmptyMVar else return mv1
    a <- compareSpecHelper m mv1 testSpec m1 m2
    b <- compareSpecHelper m mv2 testSpec m1 m2
    res1 <- readMVar mv1
    res2 <- readMVar mv2 -- if Release then mv1 == mv2 and this won't block.
    return $ getRes m res1 res2

-- | Makes sure that both Responses are the same, otherwise, if we
--   run in debug mode, an error will be thrown. If not in debug mode,
--   the first given Response (that of the theorem prover) will be
--   returned because the mistake will generally be in testSpec.
getRes :: Mode     -- ^ True if debug mode, False otherwise
       -> Response -- ^ The response from proveSpec
       -> Response -- ^ The response from testSpec
       -> Response -- ^ The final response.
getRes _        Timeout           testRes          = testRes
getRes _        Undefined         testRes          = testRes
getRes _        Equivalent        Equivalent       = Equivalent
getRes _       (NotEquivalent m) (NotEquivalent _) = NotEquivalent m
getRes Release  a                 b                = a
getRes Debug    a                 b                = error m
    where m = "proveSpec says " ++ (show a) ++ ", testSpec says " ++ (show b)

-- | Runs f on a separate thread and stores the result in mv.
compareSpecHelper m mv f m1 m2 = forkIO $ do
    res <- f m1 m2 m
    res `seq` putMVar mv res

-- Function that compares both the pre and the post condition for two methods.
-- It is assumed that both methods have the same environment (parameter names, class member names, etc).
proveSpec :: (FilePath, String) -> (FilePath, String) -> Mode -> IO Response
proveSpec method1@(_, name1) method2@(_, name2) mode = do
    let debug = mode == Debug
    -- load the methods
    m1@(decls1, mbody1, env1) <- parseMethod method1
    m2@(decls2, mbody2, env2) <- parseMethod method2
    when (decls1 /= decls2) $ fail "inconsistent class declarations"
    -- when (env1 /= env2) $ fail "inconsistent environments"
    when debug $ putStrLn $ "----PRE---- (" ++ name1 ++ " vs " ++ name2 ++ ")"
    preAns <- determineFormulaEq m1 m2 "pre" mode
    when debug $ putStrLn "\n----POST---"
    postAns <- determineFormulaEq m1 m2 "post" mode
    return $ preAns <> postAns

-- Determine the equality of two method's pre/post conditions.
determineFormulaEq :: MethodDef -> MethodDef -> String -> Mode -> IO Response
determineFormulaEq m1@(decls1, mbody1, env1) m2@(decls2, mbody2, env2) name mode = do
    let debug = mode == Debug
    -- get pre/post condition
    let (e1, e2) = (extractCond m1 name, extractCond m2 name)
    when debug $ putStrLn $ "e1:\n" ++ prettyPrint e1 ++ "\n\ne2:\n" ++ prettyPrint e2 ++ "\n"
    let (l1, l2) = (javaExpToLExpr e1 env1 decls1, javaExpToLExpr e2 env2 decls2)
    let (l, l') = (lExprPreprocessNull l1, lExprPreprocessNull l2) -- preprocess "a == null" to "isNull(a)"
    when debug $ putStrLn $ "LogicIR.Pretty 1:\n" ++ prettyLExpr l ++ "\n\nLogicIR.Pretty 2:\n" ++ prettyLExpr l' ++ "\n"
    z3Response <- l `Z3.equivalentTo` l'
    return $ toResponse z3Response
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

methodDefToLExpr :: MethodDef -> MethodDef -> String -> (LExpr, LExpr)
methodDefToLExpr m1@(decls1, _, env1) m2@(decls2, _, env2) name = do
    -- get pre/post condition
    let (e1, e2) = (extractCond m1 name, extractCond m2 name)
    let (lExpr1', lExpr2') = (javaExpToLExpr e1 env1 decls1, javaExpToLExpr e2 env2 decls2)
    -- preprocess "a == null" to "isNull(a)"
    let (lExpr1, lExpr2) = (lExprPreprocessNull lExpr1', lExprPreprocessNull lExpr2')
    (lExpr1, lExpr2)
        where extractCond m n = extractExpr $ getMethodCalls m n

testSpec :: (FilePath, String) -> (FilePath, String) -> Mode -> IO Response
testSpec method1@(_, name1) method2@(_, name2) mode = do
    let debug = mode == Debug
    m1 <- parseMethod method1
    m2 <- parseMethod method2
    when debug $ putStrLn $ "----PRE---- (" ++ name1 ++ " vs " ++ name2 ++ ")"
    let (l, l') = methodDefToLExpr m1 m2 "pre"
    preRes <- l `Test.equivalentTo` l'
    when debug $ putStrLn "\n----POST---"
    let (l, l') = methodDefToLExpr m1 m2 "post"
    postRes <- l `Test.equivalentTo` l'
    return $ (toResponse preRes) <> (toResponse postRes)

toResponse :: Z3.Z3Response -> Response
toResponse  Z3.Equivalent       = Equivalent
toResponse (Z3.NotEquivalent s) = NotEquivalent s
toResponse  Z3.Timeout          = Timeout -- TODO add QuickCheck on timeout
toResponse  _                   = Undefined
