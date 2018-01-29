module LogicIR.Backend.API where

import Z3.Monad
import Z3.Opts

import Control.Exception.Base (tryJust)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)

import ModelParser.Parser (parseModel)

import LogicIR.Backend.Z3 (lExprToZ3Ast)
import LogicIR.Backend.Pretty (showRelevantModel)
import LogicIR.Expr (LExpr)

-- | Response type.
data Z3Response = Equivalent | NotEquivalent (Maybe String) | Timeout | Undefined

-- Determine the equality of two method's pre/post conditions.
equivalentTo :: LExpr -> LExpr -> IO Z3Response
equivalentTo lexpr lexpr' = do
    let (ast1, ast2) = (lExprToZ3Ast lexpr, lExprToZ3Ast lexpr')
    ast1s <- showZ3AST ast1
    ast2s <- showZ3AST ast2
    putStrLn ast1s
    putStrLn ast2s
    res <- tryJust errorSelector (ast1 `equivalentToZ3` ast2)
    case res of
      Left () -> return Timeout
      Right (result, model) ->
        case result of
          Unsat -> return Equivalent
          Undef -> return Undefined
          Sat   -> case model of
                      Just m -> do s <- tryZ3 $ showModel m
                                   -- showRelevantModel $ parseModel s
                                   return $ NotEquivalent (Just s)
                      _ -> return $ NotEquivalent Nothing
    where
      errorSelector :: Z3Error -> Maybe ()
      errorSelector err =
        case errCode err of
          InvalidUsage -> Just ()
          _            -> Nothing

-- Check if two Z3 AST's are equivalent
equivalentToZ3 :: Z3 AST -> Z3 AST -> IO (Result, Maybe Model)
equivalentToZ3 ast1' ast2' =
  tryZ3 $ do
    ast1 <- ast1'
    ast2 <- ast2'
    astEq <- mkEq ast1 ast2
    astNeq <- mkNot astEq -- negate the question to get a model
    assert astNeq
    r <- solverCheckAndGetModel -- check in documentatie
    solverReset
    return r

-- | Z3 try evaluation with timeout.
tryZ3 = evalZ3With Nothing (Z3.Opts.opt "timeout" "5000")

-- | Pretty-print Z3 AST.
showZ3AST :: Z3 AST -> IO String
showZ3AST ast' = tryZ3 $ ast' >>= astToString
