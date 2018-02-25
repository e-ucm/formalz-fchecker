module LogicIR.Backend.Z3.API where

import qualified Z3.Base as Z3
import Z3.Monad hiding (Model)

import Control.Exception.Base (tryJust)
import Control.Monad (forM)
import Data.String
import Data.Maybe (fromJust)
import qualified Data.Map as M

import LogicIR.Backend.Z3.Z3
import LogicIR.Expr (LExpr)
import Model

-- | Determine the equality of two method's pre/post conditions.
equivalentTo :: LExpr -> LExpr -> IO Response
equivalentTo lexpr lexpr' = do
    let fv = freeVars2 lexpr lexpr'
    let (ast, ast') = (lExprToZ3Ast lexpr, lExprToZ3Ast lexpr')
    res <- tryJust errorSelector $ equivalentToZ3 fv ast ast'
    case res of
      Left () -> return Timeout
      Right r -> return r
    where
      errorSelector :: Z3Error -> Maybe ()
      errorSelector err =
        case errCode err of
          InvalidUsage -> Just ()
          _            -> Nothing

-- | Check if two Z3 AST's are equivalent.
equivalentToZ3 :: Z3 FreeVars -> Z3 AST -> Z3 AST -> IO Response
equivalentToZ3 fVars ast1' ast2' =
  tryZ3 $ do
    -- Setup
    fv <- fVars
    ast1 <- ast1'
    ast2 <- ast2'
    astEq <- mkEq ast1 ast2
    astNeq <- mkNot astEq -- negate the question to get a model

    -- Tactics
    g <- mkGoal True True False
    goalAssert g astNeq
    t <- "qe" --> "aig"
    a <- applyTactic t g
    asts <- getGoalFormulas =<< getApplyResultSubgoal a 0
    g' <- mkAnd asts
    assert g'

    -- Get model
    (r, model) <- solverCheckAndGetModel
    response <- case r of
      Unsat -> return Equivalent
      Undef -> return Undefined
      Sat   -> do
        ms <- M.fromList <$> forM (M.toList fv) (evalAST fv (fromJust model))
        return $ NotEquivalent (sanitize ms)
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

-- | Z3 try evaluation with timeout.
tryZ3 :: Z3 a -> IO a
tryZ3 = evalZ3With Nothing (  opt "timeout" (5000 :: Integer)
                           +? opt "model_validate" True
                           +? opt "well_sorted_check" True
                           +? opt "auto_config" True
                           -- +? opt "unsat_core" True
                           )

-- | Sequence tactics.
(-->) :: String -> String -> Z3 Z3.Tactic
(-->) t t' = do
  tt <- mkTactic t
  tt' <- mkTactic t'
  andThenTactic tt tt'

-- | Pretty-print Z3 AST.
showZ3AST :: Z3 AST -> IO String
showZ3AST ast' = tryZ3 $ ast' >>= astToString
