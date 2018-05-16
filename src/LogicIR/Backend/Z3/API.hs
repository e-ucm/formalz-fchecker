module LogicIR.Backend.Z3.API
       ( equivalentTo
       , showZ3AST
       , timeoutTime
       )
       where

import qualified Z3.Base  as Z3
import           Z3.Monad hiding (Model)

import           Control.Exception.Base (tryJust)
import           Control.Monad          (forM)
import qualified Data.Map               as M
import           Data.Maybe             (fromJust)
import           Data.String

import LogicIR.Backend.Z3.Z3
import LogicIR.Expr          (LExpr, lnot, (.&&), (.<==>))
import Model

-- The timeout time in milliseconds.
timeoutTime :: Integer
timeoutTime = 5000

data Z3Response = Satisfiable Model | Unsatisfiable | Undecidable
                  deriving Show

-- | Determine the equality of two method's pre/post conditions.
equivalentTo :: LExpr -> LExpr -> IO Response
equivalentTo l l' = do
  equiv <- checkZ3 $ lnot (l .<==> l')
  case equiv of
    Satisfiable m -> do
      res <- sequence $ checkZ3 <$>
        [l .&& l', l .&& lnot l', lnot l .&& l', lnot l .&& lnot l']
      let feedback = mkFeedback $ toBool <$> res
      return $ NotEquivalent m Feedback {pre = feedback, post = defFeedback}
    Unsatisfiable -> do
      res <- sequence $ checkZ3 <$> [l .&& l', lnot l .&& lnot l']
      let feedback = (head (toBool <$> res), False, False, last (toBool <$> res))
      return $ Equivalent Feedback {pre = feedback, post = defFeedback}
    Undecidable   -> return   Undefined
  where
    mkFeedback :: [Bool] -> SingleFeedback
    mkFeedback [b1, b2, b3, b4] = (b1, b2, b3, b4)
    mkFeedback _                = defFeedback
    toBool :: Z3Response -> Bool
    toBool (Satisfiable _) = True
    toBool _               = False

-- | Checks the validity of a logical statement.
checkZ3 :: LExpr -> IO Z3Response
checkZ3 l = catchTimeout $ tryZ3 $ do
  -- solverReset
  fv <- freeVars l
  ast <- lExprToZ3Ast l
  -- Get model
  (r, model) <- local $ assert ast >> solverCheckAndGetModel
  response <- case r of
    Sat   -> do
      -- Construct counter-model
      ms <- local (sanitize . M.fromList <$>
         forM (M.toList fv) (evalAST fv (fromJust model)))
      return $ Satisfiable ms
    Unsat -> return Unsatisfiable
    _ -> return Undecidable
  solverReset
  return response
  where
    -- Construct model values
    evalAST :: FreeVars -> Z3.Model -> (String, AST) -> Z3 (String, ModelVal)
    evalAST fv m (k, ast) = do
      v <- fromJust <$> modelEval m ast True
      sortKind <- getSort v >>= getSortKind
      if sortKind == Z3_ARRAY_SORT then do
        -- Retrieve array's length
        lenName <- mkStringSymbol (k ++ "?length") >>= mkIntVar
        f <- snd <$> evalAST fv m (k ++ "?length", lenName)
        let len = case f of
              (IntVal i) -> i
              _          -> error "non-int length"
        -- Iterate array "points"
        modelVals <- forM [0..(len-1)] (\i -> do
          indexAST <- mkInteger $ toInteger i
          pointAST <- mkSelect v indexAST
          snd <$> evalAST fv m ("", pointAST)
          )
        return (k, ManyVal modelVals)
      else do
        v' <- astToString v
        return (k, fromString v' :: ModelVal)


-- | Run a Z3 action in 'IO', but catch timeouts.
catchTimeout :: IO Z3Response -> IO Z3Response
catchTimeout prog = do
  res <- tryJust errorSelector prog
  return $ case res of
    Left () -> Undecidable
    Right r -> r
    where
      errorSelector :: Z3Error -> Maybe ()
      errorSelector err =
        case errCode err of
          InvalidUsage -> Just ()
          _            -> Nothing

-- | Z3 try evaluation with timeout.
tryZ3 :: Z3 a -> IO a
tryZ3 prog = do
  env <- newEnv Nothing (  opt "timeout" timeoutTime
                        +? opt "model_validate" True
                        +? opt "well_sorted_check" True
                        +? opt "auto_config" True
                        -- +? opt "unsat_core" True
                        )
  evalZ3WithEnv prog env

-- | Pretty-print Z3 AST.
showZ3AST :: Z3 AST -> IO String
showZ3AST ast' = tryZ3 $ ast' >>= astToString
