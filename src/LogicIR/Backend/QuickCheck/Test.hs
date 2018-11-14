module LogicIR.Backend.QuickCheck.Test ( testEquality
                                       , LExprTeacher
                                       , LExprStudent
                                       , FeedbackCount
                                       , equal
                                       ) where

import JavaHelpers.HelperFunctions (fromJust')
import LogicIR.Expr
import LogicIR.Fold
import LogicIR.Eval
import LogicIR.Backend.QuickCheck.ModelGenerator
import Data.Maybe (fromMaybe, isNothing)
import Data.List (find, (\\))
import qualified Data.Map.Lazy as M
import Control.Concurrent.Async (mapConcurrently)

-- | Contains how often any type of feedback occured over x runs (where x is
--   obviously the sum of the four integers). Order of Integers in tuple
--   is the same as in Model.SingleFeedback
type FeedbackCount = ( Int -- T-T
                     , Int -- T-F
                     , Int -- F-T
                     , Int -- F-F
                     )

defFbCount :: FeedbackCount
defFbCount = (0,0,0,0)

add :: FeedbackCount -> FeedbackCount -> FeedbackCount
add (w,x,y,z) (w',x',y',z') = (w+w',x+x',y+y',z+z')

-- | Converts two Bools to FeedbackCount. First Bool is the Bool you get when
--   the TeacherLExpr was evaluated using some Model+ArrayModel m, the second
--   one the bool you get when you evaluate the StudentLExpr using m.
toFeedbackCount :: Bool -> Bool -> FeedbackCount
toFeedbackCount True  True  = (1,0,0,0)
toFeedbackCount True  False = (0,1,0,0)
toFeedbackCount False True  = (0,0,1,0)
toFeedbackCount False False = (0,0,0,1)

-- | Returns True if, on we have 0 TF or FT occurences (in other words: occurences
--   where the teacher and student specification gave a different result)
equal :: FeedbackCount -> Bool
equal (_,tf,ft,_) = tf == 0 && ft == 0

-- Calls 'test' multiple times.
testEquality :: Int -> LExprTeacher -> LExprStudent -> IO (FeedbackCount, (Model, ArrayModel))
testEquality 0 _ _ = error "testEquality won't work with 0 iterations."
testEquality iters e1 e2 = do
  results <- testAsync iters e1 e2 --  mapM (const (test e1 e2)) [1..iters]
  let feedback = foldr (add . fst) defFbCount results -- sum of feedbacks
  -- get list of counter models where
  let counters = map snd (filter (not . equal . fst) results)
  if null counters then
    return (feedback, ([], M.empty))
  else
    return (feedback, head counters)

testAsync :: Int -> LExprTeacher -> LExprStudent -> IO [(FeedbackCount, (Model, ArrayModel))]
testAsync iters e1 e2 = do
  let granularity = 50 :: Double
  let nrThreads = ceiling (fromIntegral iters / granularity) :: Integer
  let threads = [1..nrThreads]
  results <- mapConcurrently (const (mapM (const (test e1 e2)) [(1::Integer)..50])) threads
  return $ concat results

test :: LExprTeacher -> LExprStudent -> IO (FeedbackCount, (Model, ArrayModel))
test e1 e2 = do
    (CBool res1, (m,arrM,_)) <- testLExpr e1
    evalInfo'                <- expandEvalInfo e2 (m, arrM)
    (CBool res2)             <- testModel e2 evalInfo'
    let (primitivesModel, arrayModel, _) = evalInfo'
    let fb = toFeedbackCount res1 res2
    return (fb, (primitivesModel, arrayModel))

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
applyModel e evalInfo = foldLExpr (LAlgebra fcns fvar funi fbin fiff fqnt farr fnll flen) e
    where fcns            = LConst
          funi op x       = evalIfPossible $ LUnop op x
          fbin le op re   = evalIfPossible $ LBinop le op re
          fiff ge e1 e2   = evalIfPossible $ LIf ge e1 e2
          fvar x          = subst (LVar x) evalInfo
          fqnt op x e1 e2 = subst (LQuant op x (subst e1 evalInfo) (subst e2 evalInfo)) evalInfo
          farr x i        = subst (LArray x i) evalInfo
          fnll x          = subst (LIsnull x) evalInfo
          flen x          = subst (LLen x) evalInfo

get :: (Ord a, Show b, Show a) => a -> M.Map a [b] -> [b]
get a model =
  fromMaybe
    (error $ "Impossible substitution of Array expression. Array: " ++ show a ++ ", ArrayModel: " ++ show model)
    (M.lookup a model)

-- Substitutes an LExpr if a valid subst can be found in the given models.
-- Otherwise simply returns the input expression as output.
subst :: LExpr -> EvalInfo -> LExpr
subst e@(LArray x indexE) (_,arrayModel,_)
    = if evalPossible indexE then do
          let (CInt index) = eval indexE
          let a            = get x arrayModel
          let l          = length a
          if index < l && index >= 0 then
              LConst $ a !! index
          else
              e -- Just return the input LExpr, since the index is out of the domain.
      else
          e
subst q@(LQuant qop x domain e) evalInfo = do
    let op = if qop == QAll then and else or
    let results = evalQuantifier x domain e evalInfo
    if any isNothing results
    then
        q
    else
        LConst $ CBool $ op $ map (fromJust' "subst") results
subst (LLen x) (_, arrayModel, _) = LConst (CInt (length (get x arrayModel)))
subst x (model,_,_)               = maybe x snd $ find (\(old, _) -> x == old) model


-- Returns, given a domain LExpr that concerns a certain Var, all
-- Ints that fall in that domain.
evalQuantifier :: Var -> LExpr -> LExpr -> EvalInfo -> [Maybe Bool]
evalQuantifier x domain e (m1,m2,quantVs) = do
    -- Check which integers in domain [-20,20] fall in the domain of the
    -- quantifier and evaluate the quantifier with those integers.
    let newM1s = map (\i -> (LVar x, LConst (CInt i)) : m1) [-20..20]
    concatMap getResult newM1s
    where isTrue expr = eval expr == CBool True
          getResult newM1 = do
              let newEvalInfo = (newM1,m2,quantVs)
              let newDomain = applyModel domain newEvalInfo
              if evalPossible newDomain then
                  if isTrue newDomain then do
                      let newE = applyModel e newEvalInfo
                      if evalPossible newE && isTrue newE then
                        [Just True]
                      else
                        -- This can happen if newDomain is true but the actual quantifier expression can not be evaluated.
                        -- e.g.   a.length==1 && forall 0<i<2: a[i]<a[i+1] (here, a[i+1] == 2, but a is only 1 in length)
                        -- We view such quantifier iterations as failures, i.e. False.
                        [Just False]
                  else
                      [] -- domain expression is false, so we skip the iteration.
              else do
                  let (primitives,(_,_),_) = findVariables newDomain
                  -- If null == True, then there is a primitive in the newDomain that is bound by a parent/ancestor quantifier.
                  if null (primitives \\ quantVs) then
                      -- newDomain could not be evaluated because it is nested in another quantifier that has
                      -- not been evaluated yet. Therefore, we just wait with the evaluation of this quantifier.
                      [Nothing]
                  else
                      -- newDomain could not be evaluated because there are variables in it that should have been
                      -- substituted by now. This should never happen.
                      error $ "Quantifier domain could not be evaluated. This should never happen. " ++ show newDomain
