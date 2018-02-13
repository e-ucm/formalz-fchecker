module LogicIR.Backend.QuickCheck.Test (testEquality) where

import LogicIR.Expr
import LogicIR.Fold
import LogicIR.Eval
import LogicIR.Backend.QuickCheck.ModelGenerator
import Data.Maybe (fromMaybe, isNothing, fromJust)
import Data.List (find, nub, (\\))
import Data.Map (Map)
import qualified Data.Map.Lazy as M

test :: LExpr -> LExpr -> IO (Bool, Model, ArrayModel)
test e1 e2 = do
    (res1,(m,arrM,_)) <- testLExpr e1
    evalInfo'         <- expandEvalInfo e2 (m, arrM)
    res2              <- testModel e2 evalInfo'
    let (primitivesModel, arrayModel, _) = evalInfo'
    return (res1 == res2, primitivesModel, arrayModel)

-- Calls 'test' multiple times.
testEquality :: Int -> LExpr -> LExpr -> IO (Bool, (Model, ArrayModel))
testEquality 0 e1 e2 = do
    return (True, ([], M.empty))
testEquality n e1 e2 = do
    (r,m1,m2) <- test e1 e2
    if not r then do
        return (False, (m1, m2))
    else do
        (rs, model) <- testEquality (n-1) e1 e2
        return ((r && rs), model)

-- Given an LExpr, generates a substitution model for all variables in it,
-- and returns whether the formula evaluates to true or to false when that model
-- is used, and the model itself. If the formula, after substitution, cannot
-- be evaluated - i.e. there's still a variable in the LExpr - an error will be thrown.
testLExpr :: LExpr -> IO (LConst, EvalInfo)
testLExpr e = do
    (primitivesModel, arrayModel, quantVs) <- generateEvalInfo e
    testResult       <- testModel e (primitivesModel, arrayModel, quantVs)
    let evalInfo      = (primitivesModel, arrayModel, quantVs)
    return (testResult, evalInfo)

testModel :: LExpr -> EvalInfo -> IO LConst
testModel e evalInfo = do
    let substitutedE = applyModel e evalInfo
    if evalPossible substitutedE then do
        let res = eval substitutedE
        return res
    else
        return $ CBool False

-- Applies substitution to an LExpr given the necessary EvalInfo.
applyModel :: LExpr -> EvalInfo -> LExpr
applyModel e evalInfo = foldLExpr algebra e
    where algebra :: LExprAlgebra LExpr
          algebra = (cnst, var, uni, bin, iff, quant, arr, snull, len)
          cnst             = LConst
          uni op e         = evalIfPossible $ LUnop op e
          bin le op re     = evalIfPossible $ LBinop le op re
          iff ge e1 e2     = evalIfPossible $ LIf ge e1 e2
          var v            = subst (LVar v) evalInfo
          quant op v e1 e2 = subst (LQuant op v (subst e1 evalInfo) (subst e2 evalInfo)) evalInfo
          arr v e          = subst (LArray v e) evalInfo
          snull v          = subst (LIsnull v) evalInfo
          len v            = subst (LLen v) evalInfo

get arr model = do
    let res = (M.lookup) arr model
    if res == Nothing then do
        error $ "Impossible substitution of Array expression. Array: " ++ show arr ++ ", ArrayModel: " ++ show model
        []
    else do
        fromJust res

-- Substitutes an LExpr if a valid subst can be found in the given models.
-- Otherwise simply returns the input expression as output.
subst :: LExpr -> EvalInfo -> LExpr
subst e@(LArray v indexE) (_,arrayModel,_)
    = if evalPossible indexE then do
          let (CInt index) = eval indexE
          let a            = get v arrayModel
          let len          = length a
          if index < len && index >= 0 then
              LConst $ a !! index
          else
              e -- Just return the input LExpr, since the index is out of the domain.
      else do
          e
subst   q@(LQuant qop v domain e) evalInfo = do
    let op = if qop == QAll then and else or
    let results = evalQuantifier v domain e evalInfo
    if any isNothing results
    then
        q
    else
        LConst $ CBool $ op $ map fromJust results
subst (LLen    v)  (_,arrayModel,_) = LConst (CInt (length (get v arrayModel)))
subst v (model,_,_)                 = fromMaybe v (fmap snd $ find (\(old,new) -> v == old) model)


-- Returns, given a domain LExpr that concerns a certain Var, all
-- Ints that fall in that domain.
evalQuantifier :: Var -> LExpr -> LExpr -> EvalInfo -> [Maybe Bool]
evalQuantifier v domain e (m1,m2,quantVs) = do
    -- Check which integers in domain [-20,20] fall in the domain of the
    -- quantifier and evaluate the quantifier with those integers.
    let newM1s = map (\i -> (LVar v, LConst (CInt i)) : m1) [-20..20]
    concatMap getResult newM1s
    where isTrue expr = (eval expr) == (CBool True)
          getResult newM1 = do
              let newEvalInfo = (newM1,m2,quantVs)
              let newDomain = applyModel domain newEvalInfo
              if evalPossible newDomain then
                  if isTrue newDomain then do
                      let newE = applyModel e newEvalInfo
                      if evalPossible newE then
                          if isTrue newE then
                              [Just True]
                          else
                              [Just False]
                      else do
                          -- This can happen if newDomain is true but the actual quantifier expression can not be evaluated.
                          -- e.g.   a.length==1 && forall 0<i<2: a[i]<a[i+1] (here, a[i+1] == 2, but a is only 1 in length)
                          -- We view such quantifier iterations as failures, i.e. False.
                          [Just False]
                  else
                      [] -- domain expression is false, so we skip the iteration.
              else do
                  let vs@(primitives,(_,_),_) = findVariables newDomain
                  -- If null == True, then there is a primitive in the newDomain that is bound by a parent/ancestor quantifier.
                  if null (primitives \\ quantVs) then
                      -- newDomain could not be evaluated because it is nested in another quantifier that has
                      -- not been evaluated yet. Therefore, we just wait with the evaluation of this quantifier.
                      [Nothing]
                  else
                      -- newDomain could not be evaluated because there are variables in it that should have been
                      -- substituted by now. This should never happen.
                      error $ "Quantifier domain could not be evaluated. This should never happen. " ++ (show newDomain)
