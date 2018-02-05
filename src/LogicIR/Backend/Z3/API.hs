module LogicIR.Backend.Z3.API where

import Z3.Monad
import Z3.Opts

import Control.Exception.Base (tryJust)
import Control.Monad (when, forM_)
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))

import LogicIR.Backend.Z3.Model

import LogicIR.Backend.Z3.Pretty (showRelevantModel)
import LogicIR.Backend.Z3.Z3
import LogicIR.Expr (LExpr)

-- | Z3 Response type.
data Z3Response = Equivalent | NotEquivalent (Maybe String) | Timeout | Undefined

-- | Determine the equality of two method's pre/post conditions.
equivalentTo :: LExpr -> LExpr -> IO Z3Response
equivalentTo lexpr lexpr' = do
    let (ast1, ast2) = (lExprToZ3Ast lexpr, lExprToZ3Ast lexpr')
    ast1s <- showZ3AST ast1
    ast2s <- showZ3AST ast2
    putStrLn $ "Z3_AST1:\n" ++ ast1s ++ "\n\nZ3_AST2:\n" ++ ast2s ++ "\n"
    res <- tryJust errorSelector (ast1 `equivalentToZ3` ast2)
    case res of
      Left () -> return Timeout
      Right (result, model) ->
        case result of
          Unsat -> return Equivalent
          Undef -> return Undefined
          Sat   -> do modelStr <- tryZ3 $ showModel $ fromJust model
                      putStrLn $ "\nModel:\n" ++ modelStr
                      case runP' modelP modelStr of
                        Left err -> putStrLn $ "Cannot parse model:\n" ++ show err
                        Right m -> showRelevantModel m
                      return $ NotEquivalent (Just modelStr)
    where
      errorSelector :: Z3Error -> Maybe ()
      errorSelector err =
        case errCode err of
          InvalidUsage -> Just ()
          _            -> Nothing

-- | Sequence tactics.
(-->) t t' = do
  tt <- mkTactic t
  tt' <- mkTactic t'
  andThenTactic tt tt'

-- | Check if two Z3 AST's are equivalent.
equivalentToZ3 :: Z3 AST -> Z3 AST -> IO (Result, Maybe Model)
equivalentToZ3 ast1' ast2' =
  tryZ3 $ do
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
    liftIO $ putStrLn "After tactics:"
    astStrs <- mapM astToString asts
    forM_ astStrs (liftIO . putStrLn)
    g' <- mkAnd asts
    assert g'

    -- assert astNeq
    r <- solverCheckAndGetModel -- check in documentation
    solverReset
    return r

-- | Z3 try evaluation with timeout.
tryZ3 = evalZ3With Nothing (  opt "timeout" (5000 :: Integer)
                           +? opt "model_validate" True
                           +? opt "well_sorted_check" True
                           +? opt "auto_config" True
                           -- +? opt "unsat_core" True
                           )

-- | Pretty-print Z3 AST.
showZ3AST :: Z3 AST -> IO String
showZ3AST ast' = tryZ3 $ ast' >>= astToString
