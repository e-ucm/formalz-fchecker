module API where

import Control.Concurrent
import Data.Maybe

import Javawlp.Engine.HelperFunctions
import Javawlp.Engine.Types
import Language.Java.Syntax

import qualified LogicIR.Backend.QuickCheck.API as Test
import qualified LogicIR.Backend.Z3.API as Z3
import LogicIR.Expr
import LogicIR.Frontend.Java (javaExpToLExpr)
import LogicIR.Null (lExprPreprocessNull)
import Model

-- | Data types.
data Mode = Debug | Release deriving (Eq, Show)

data ParseMode = Raw | File

type Source = String

type EquivImpl = LExpr -> LExpr -> IO Response

type MethodDef = ([TypeDecl], Stmt, TypeEnv)

-- | Calls proveSpec and testSpec on different threads and returns
--   their response. If the function is called in debug mode, it
--   compares the result of testSpec and proveSpec. Otherwise, it
--   returns the answer of the fastest method of the two.
compareSpec :: Mode               -- ^ The execution mode
            -> ParseMode          -- ^ The parsing mode
            -> (FilePath, String) -- ^ Specification 1
            -> (FilePath, String) -- ^ Specification 2
            -> IO Response        -- ^ Result of spec comparison.
compareSpec m pMode methodA methodB = do
    mv1 <- newEmptyMVar
    mv2 <- if m == Debug then newEmptyMVar else return mv1
    _ <- compareSpecHelper mv1 (checkSpec pMode Z3.equivalentTo)
    _ <- compareSpecHelper mv2 (checkSpec pMode Test.equivalentTo)
    res1 <- readMVar mv1
    res2 <- readMVar mv2 -- if Release then mv1 == mv2 and this won't block
    return $ getRes m res1 res2
    where -- | Runs f on a separate thread and stores the result in mv.
          compareSpecHelper mv f = forkIO $ do
            res <- f methodA methodB
            res `seq` putMVar mv res

-- | Makes sure that both Responses are the same, otherwise, if we
--   run in debug mode, an error will be thrown. If not in debug mode,
--   the first given Response (that of the theorem prover) will be
--   returned because the mistake will generally be in testSpec.
getRes :: Mode     -- ^ True if debug mode, False otherwise
       -> Response -- ^ The response from proveSpec
       -> Response -- ^ The response from testSpec
       -> Response -- ^ The final response.
getRes _        Timeout           testRes           = testRes
getRes _        Undefined         testRes           = testRes
getRes _        Equivalent        Equivalent        = Equivalent
getRes _        (NotEquivalent m) (NotEquivalent _) = NotEquivalent m
getRes Release  resp              _                 = resp
getRes Debug    resp              resp'             =
    error $ "proveSpec says " ++ show resp ++ ", testSpec says " ++ show resp'

checkSpec :: ParseMode -> EquivImpl -> (Source, String) -> (FilePath, String) -> IO Response
checkSpec pMode equivTo methodA methodB = do
    [m1, m2] <- mapM (parseMethod pMode) [methodA, methodB]
    let (preL, preL') = methodDefToLExpr m1 m2 "pre"
    preRes <- preL `equivTo` preL'
    let (postL, postL') = methodDefToLExpr m1 m2 "post"
    postRes <- postL `equivTo` postL'
    return $ preRes <> postRes

--------------------------------------------------------------------------------

-- Takes a Java source file and a method name and returns the class declarations,
-- Returns the method body and the method's formal parameters.
parseMethod :: ParseMode -> (Source, String) -> IO MethodDef
parseMethod pMode (src, name) = do
    decls <- case pMode of
                Raw  -> parseDeclsRaw src
                File -> parseDecls src
    -- get the method's body (assuming all methods have different names)
    let mbody = fromJust $ getMethod decls (Ident name)
    -- get the method's formal parameters:
    let env = getMethodTypeEnv decls (Ident name)
    -- return the relevant data
    return (decls, mbody, env)

methodDefToLExpr :: MethodDef -> MethodDef -> String -> (LExpr, LExpr)
methodDefToLExpr m1@(decls1, _, env1) m2@(decls2, _, env2) name = do
    -- get pre/post condition
    let (e1, e2) = (extractCond m1 name, extractCond m2 name)
    let (l1, l2) = (javaExpToLExpr e1 env1 decls1, javaExpToLExpr e2 env2 decls2)
    -- preprocess "a == null" to "isNull(a)"
    let (l, l') = (lExprPreprocessNull l1, lExprPreprocessNull l2)
    (l, l')
        where extractCond :: MethodDef -> String -> Exp
              extractCond m n = extractExpr $ getMethodCalls m n

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
