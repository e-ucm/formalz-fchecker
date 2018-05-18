module LogicIR.Backend.QuickCheck.API (equivalentTo) where

import           Control.Arrow                             ((***))
import qualified Data.Map.Lazy                             as M
import qualified LogicIR.Backend.QuickCheck.ModelGenerator as QC
import           LogicIR.Backend.QuickCheck.Test
import           LogicIR.Expr                              hiding (b,r,n)
import           LogicIR.Pretty                            (prettyLExpr)
import           LogicIR.Backend.QuickCheck.Iterations     (iterations)
import           Model

-- | Determine the equality of two method's pre/post conditions.
equivalentTo :: LExprTeacher -> LExprStudent -> IO Response
equivalentTo lexpr lexpr' = do
    let iters = max (iterations lexpr) (iterations lexpr')
    (feedbackCount, counterModel) <- testEquality iters lexpr lexpr'
    let fb = toSingleFeedback feedbackCount
    let feedback = Feedback { pre = fb, post = defFeedback }
    if equal feedbackCount then
      return $ Equivalent feedback
    else do
      return $ NotEquivalent (toZ3Model counterModel) feedback

toSingleFeedback :: FeedbackCount -> SingleFeedback
toSingleFeedback (a,b,c,d) = (n a, n b, n c, n d)
  where n = (0<)

toZ3Model :: (QC.Model, QC.ArrayModel) -> Model
toZ3Model (m, arrayM) =
    M.fromList $
      map (\(k,val) -> (removeType k,val)) $
        map (prettyLExpr *** toModelVal) m ++
        map ((prettyLExpr *** toModelVals) . (LVar *** fmap LConst)) (M.toList arrayM)

toModelVal :: LExpr -> ModelVal
toModelVal (LConst (CBool b)) = BoolVal b
toModelVal (LConst (CInt  i)) = IntVal  $ toInteger i
toModelVal (LConst (CReal r)) = RealVal r
toModelVal _                  = error "toModelVal: invalid LExpr"

toModelVals :: [LExpr] -> ModelVal
toModelVals = ManyVal . map toModelVal
