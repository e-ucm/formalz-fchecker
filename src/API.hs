module API where

import Control.Concurrent
import Data.List          (intercalate)
import Data.List.Split    (splitOn)
import Data.Maybe
import GHC.Generics
import Prelude            hiding (log)
import System.IO          (hPutStrLn, stderr)
import System.Timeout

import JavaHelpers.HelperFunctions
import JavaHelpers.Types
import Language.Java.Pretty
import Language.Java.Syntax

import qualified LogicIR.Backend.QuickCheck.API as Test
import qualified LogicIR.Backend.Z3.API         as Z3
import           LogicIR.Expr
import           LogicIR.Frontend.Java          (javaExpToLExpr)
import           LogicIR.Preprocess             (preprocess)
import           LogicIR.Pretty
import           LogicIR.TypeChecker            (typeCheck)
import           Model

import Control.DeepSeq
import Control.Exception.Base

-- | Data types.
data Options = Options { mode      :: Mode      -- ^ The execution mode
                       , parseMode :: ParseMode -- ^ The parsing mode
                       , checkPre  :: Bool      -- ^ check pre-condition?
                       , checkPost :: Bool      -- ^ check post-condition?
                       }
                       deriving (Eq, Show, Generic)

defOptions :: Options
defOptions = Options Release Raw True True

-- | Soft debug mode means that the results from Z3 and QuickCheck will be
--   compared (Equiv / NEquiv), but not the Feedback. This Mode is used when
--   debugging programs with reals, because QuickCheck is not very accurate
--   when there are reals involved.
data Mode = Z3 | SoftDebug | Debug | Release
            deriving (Eq, Show, Generic)

data ParseMode = Raw | File
                 deriving (Eq, Show, Generic)

type Source = String

type MethodDef = ([TypeDecl], Stmt, TypeEnv)

type EquivImpl = LExpr -> LExpr -> IO Response

-- | Calls proveSpec and testSpec on different threads and returns
--   their response. If the function is called in debug mode, it
--   compares the result of testSpec and proveSpec. Otherwise, it
--   returns the answer of the fastest method of the two.
compareSpec :: Options            -- ^ Comparison options
            -> (FilePath, String) -- ^ Specification 1
            -> (FilePath, String) -- ^ Specification 2
            -> IO Response        -- ^ Result of spec comparison.
compareSpec opts@(Options m pMode toPre toPost) methodA methodB = do
  -- Parsing.
  [mA, mB] <- mapM (parseMethod pMode) [methodA, methodB]

  log $ "Input\n" ++ "~~~~\n"
  log $ "MethodA:\n" ++ ppMethodDef mA ++ "\n"
  log $ "MethodB:\n" ++ ppMethodDef mB ++ "\n"
  log $ "Options:\n" ++ show opts
  log $ "Pre\n" ++ "~~~\n"
  res <- methodDefToLExpr mA mB "pre"

  case res of
    Left e -> do
      log $ "*** ERROR: " ++ show e
      return $ mkErrorResponse e

    Right (preL, preL') -> do
      log $ "LExprA:\n" ++ prettyLExpr preL ++ "\n"
      log $ "LExprB:\n" ++ prettyLExpr preL' ++ "\n"
      log $ "Post\n" ++ "~~~~\n"

      res' <- methodDefToLExpr mA mB "post"
      case res' of
        Left e' -> do
          log $ "*** ERROR: " ++ show e'
          return $ mkErrorResponse e'
        Right (postL, postL') -> do
          log $ "Result\n" ++ "~~~~\n"
          log $ "LExprA:\n" ++ prettyLExpr postL ++ "\n"
          log $ "LExprB:\n" ++ prettyLExpr postL' ++ "\n"

          -- Run Z3 with mv1 and Test with mv2. If Release, they both get the same
          -- mvar and whoever finishes first will therefore write to both
          -- mv1 and mv2. If Debug/SoftDebug, they both get a different MVar, we wait for
          -- both and then compare their results.
          resp <-
            if m == Z3 then
              checkEquiv toPre toPost "Z3" Z3.equivalentTo (preL, preL') (postL, postL')
            else do mv1 <- newEmptyMVar
                    mv2 <- if m == Release then return mv1 else newEmptyMVar
                    mapM_ compareSpecHelper [ (mv1, "Z3",   Z3.equivalentTo)
                                            , (mv2, "Test", Test.equivalentTo)
                                            ]
                    waitForResult m mv1 mv2

          log $ "FullReponse: " ++ show resp
          let finalResp = minifyResponse (tenvA ++ tenvB) resp
          log $ "FinalResponse: " ++ show finalResp
          return finalResp

          where
            ((_, _, tenvA), (_, _, tenvB)) = (mA, mB)

            -- | Runs f on a separate thread and stores the result in mv.
            compareSpecHelper :: (MVar Response, String, EquivImpl) -> IO ThreadId
            compareSpecHelper (mv, name, impl) = forkIO $ do
              resp <- checkEquiv toPre toPost name impl (preL, preL') (postL, postL')
              resp `seq` putMVar mv resp

-- Exclude internal variables in Response's model.
minifyResponse :: TypeEnv -> Response -> Response
minifyResponse tenv (NotEquivalent md fb) = NotEquivalent (minify tenv md) fb
minifyResponse _    res                   = res

-- Waits (blocking) until it has a Response to return.
waitForResult :: Mode -> MVar Response -> MVar Response -> IO Response
waitForResult Release = waitForResultRelease
waitForResult m       = waitForResultDebug m

-- | Run both `Z3` and `Test`, wait for their responses (no matter how slow they
--   are) and compare them.
waitForResultDebug :: Mode -- ^ Either `Debug` or `SoftDebug`
                   -> MVar Response -> MVar Response -> IO Response
waitForResultDebug debugMode z3mv testmv = do
  z3res   <- readMVar z3mv
  testres <- readMVar testmv
  return $ getRes debugMode z3res testres

-- | Try to compare using Z3 first. If timeout is reached, fall back on Test. If
--   test is too slow as well, give up.
waitForResultRelease :: MVar Response -> MVar Response -> IO Response
waitForResultRelease z3mv testmv = do
      let t      = fromIntegral (Z3.timeoutTime * 1000) -- 1 ms == 1000 μs
      maybeZ3Res <- timeout t (readMVar z3mv)
      case maybeZ3Res of
        Nothing -> do -- Z3 too slow
          maybeTestRes <- timeout t (readMVar testmv)
          case maybeTestRes of
            Nothing -> return Timeout -- test too slow
            Just m  -> return m -- test fast enough
        Just m -> return m -- Z3 fast enough

-- | Makes sure that both Responses are the same, otherwise, if we
--   run in debug mode, an error will be thrown. If not in debug mode,
--   the first given Response (that of the theorem prover) will be
--   returned because the mistake will generally be in testSpec.
getRes :: Mode     -- ^ True if debug mode, False otherwise
       -> Response -- ^ The response from proveSpec (Z3)
       -> Response -- ^ The response from testSpec  (Testing library)
       -> Response -- ^ The final response.
getRes m Timeout testRes
  | m == Release = testRes
  | otherwise    = error $ "proveSpec timed out, testSpec says " ++ show testRes
getRes m Undefined testRes
  | m == Release = testRes
  | otherwise    = error $ "proveSpec says Undefined, testSpec says " ++ show testRes
getRes m (Equivalent f1) (Equivalent f2)
  | m == Debug && f1 == f2 = Equivalent f1
  | m == Debug             = error $ "testSpec says " ++ show (Equivalent f2) ++
                                     " but it should've been " ++ show (Equivalent f1)
  | otherwise              = Equivalent f1
getRes m (NotEquivalent m1 f1) (NotEquivalent m2 f2)
  | m == Debug && f1 == f2 = NotEquivalent m1 f1
  | m == Debug             = error $ "testSpec says " ++ show (NotEquivalent m2 f2) ++
                                     " but it should've been " ++ show (NotEquivalent m1 f1)
  | otherwise = NotEquivalent m1 f1
getRes Release  resp                _                   = resp
getRes _        resp                resp'               =
  error $ "proveSpec says " ++ show resp ++ ", testSpec says " ++ show resp'

-- | Check if two logic statements are equivalent.
checkEquiv :: Bool -> Bool -> String -> EquivImpl
          -> (LExpr, LExpr) -> (LExpr, LExpr) -> IO Response
checkEquiv toPre toPost name equivTo (preL, preL') (postL, postL') = do
  let ignore = return $ Equivalent defFeedback'
  preRes  <- if toPre then  preL `equivTo` preL'   else ignore
  postRes <- if toPost then postL `equivTo` postL' else ignore
  log $ "PreResponse (" ++ name ++ "):\n" ++ show preRes ++ "\n"
  log $ "PostResponse (" ++ name ++ "):\n" ++ show postRes ++ "\n"
  return $ preRes <> postRes

--------------------------------------------------------------------------------

-- Takes a Java source file and a method name and returns the class declarations,
-- Returns the method body and the method's formal parameters.
parseMethod :: ParseMode -> (Source, String) -> IO MethodDef
parseMethod pMode (src, name) = do
  decls <- case pMode of
              Raw  -> return (parseDeclsRaw src)
              File -> parseDecls src
  -- get the method's body (assuming all methods have different names)
  let mbody = fromJust' "parseMethod" $ getMethod decls (Ident name)
  -- get the method's formal parameters:
  let env = getMethodTypeEnv decls (Ident name)
  -- return the relevant data
  return (decls, mbody, env)

methodDefToLExpr :: MethodDef -> MethodDef -> String -> IO (Either String (LExpr, LExpr))
methodDefToLExpr m1@(decls1, _, env1) m2@(decls2, _, env2) name = do
  -- get pre/post condition
  let (e1, e2) = (extractCond m1 name, extractCond m2 name)
  res :: Either SomeException (LExpr, LExpr) <-
    try . evaluate . force $ (javaExpToLExpr e1 env1 decls1, javaExpToLExpr e2 env2 decls2)
  return $ case res of
    Left e ->
      Left $ "methodDefToLExpr: " ++ show e
    Right (l, l') -> do
      tl <- typeCheck $ preprocess l
      tl' <- typeCheck $ preprocess l'
      return (tl, tl')
  where extractCond :: MethodDef -> String -> Exp
        extractCond m x = extractExpr $ getMethodCalls m x

-- Get a list of all calls to a method of a specific name from a method definition.
getMethodCalls :: MethodDef -> String -> [MethodInvocation]
getMethodCalls (_, StmtBlock (Block bs), _) name = mapMaybe extractMethodInv bs
  where
    extractMethodInv :: BlockStmt -> Maybe MethodInvocation
    extractMethodInv (BlockStmt (ExpStmt (MethodInv i@(MethodCall (Name [Ident x]) _)))) =
      if x == name then
        Just i else
      Nothing
    extractMethodInv _ = Nothing
getMethodCalls _ _ = error "getMethodCalls: invalid arguments"

-- [pre(a), pre(b), pre(c)] -> (a AND b AND c)
extractExpr :: [MethodInvocation] -> Exp
extractExpr call =
  combineExprs $ concatMap (\(MethodCall (Name [Ident _]) args) -> args) call
  where
    combineExprs :: [Exp] -> Exp
    combineExprs []     = true
    combineExprs [e]    = e
    combineExprs (e:es) = e &* combineExprs es

--------------------------------- Debugging ------------------------------------
log :: String -> IO ()
log = hPutStrLn stderr

mkErrorResponse :: String -> Response
mkErrorResponse = ErrorResponse . head . splitOn "\nCallStack"

ppMethodDef :: MethodDef -> String
ppMethodDef (_, stmt, typeEnv) =
  ppTypeEnv typeEnv ++ "\n" ++ prettyPrint stmt

ppTypeEnv :: TypeEnv -> String
ppTypeEnv = intercalate ", " . map ppNT
  where ppNT (x, t) = prettyPrint t ++ " " ++ prettyPrint x
