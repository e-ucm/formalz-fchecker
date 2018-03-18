module LogicIR.Backend.QuickCheck.API (equivalentTo) where

import           Control.Arrow                             ((***))
import qualified Data.Map.Lazy                             as M
import qualified LogicIR.Backend.QuickCheck.ModelGenerator as QC
import           LogicIR.Backend.QuickCheck.Test
import           LogicIR.Expr                              hiding (b, r)
import           LogicIR.Pretty                            (prettyLExpr)
import           Model

-- | Determine the equality of two method's pre/post conditions.
equivalentTo :: LExpr -> LExpr -> IO Response
equivalentTo lexpr lexpr' = do
    (eq, testModel) <- testEquality 1000 lexpr lexpr'
    let feedback = Feedback { pre = defFeedback -- TODO check which combinations are possible
                            , post = defFeedback -- leave as is
                            }
    if eq
    then return Equivalent
    else return $ NotEquivalent (toZ3Model testModel) feedback

toZ3Model :: (QC.Model, QC.ArrayModel) -> Model
toZ3Model (m, arrayM) =
    M.fromList $
      map (prettyLExpr *** toModelVal) m ++
      map ((prettyLExpr *** toModelVals) . (LVar *** fmap LConst)) (M.toList arrayM)

toModelVal :: LExpr -> ModelVal
toModelVal (LConst (CBool b)) = BoolVal b
toModelVal (LConst (CInt  i)) = IntVal  $ toInteger i
toModelVal (LConst (CReal r)) = RealVal r
toModelVal _                  = error "toModelVal: invalid LExpr"

toModelVals :: [LExpr] -> ModelVal
toModelVals = ManyVal . map toModelVal
