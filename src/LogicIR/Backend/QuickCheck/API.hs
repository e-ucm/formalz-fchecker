module LogicIR.Backend.QuickCheck.API (equivalentTo) where

import Control.Arrow ((***))
import Data.Map (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe (fromJust, fromMaybe, isNothing)
import qualified LogicIR.Backend.QuickCheck.ModelGenerator as QC
import LogicIR.Backend.QuickCheck.Test
import LogicIR.Expr
import LogicIR.Pretty (prettyLExpr)
import Model

-- | Determine the equality of two method's pre/post conditions.
equivalentTo :: LExpr -> LExpr -> IO Response
equivalentTo lexpr lexpr' = do
    (eq, testModel) <- testEquality 1000 lexpr lexpr'
    let feedback = NoFeedback -- TODO check implication (stronger + weaker)
    if eq
    then return Equivalent
    else return $ NotEquivalent (toZ3Model testModel) (feedback, NoFeedback)

toZ3Model :: (QC.Model, QC.ArrayModel) -> Model
toZ3Model (m, arrayM) =
    M.fromList $
      map (prettyLExpr *** toModelVal) m ++
      map ((prettyLExpr *** toModelVals) . (LVar *** fmap LConst)) (M.toList arrayM)

toModelVal :: LExpr -> ModelVal
toModelVal (LConst (CBool b)) = BoolVal b
toModelVal (LConst (CInt  i)) = IntVal  $ toInteger i
toModelVal (LConst (CReal r)) = RealVal r

toModelVals :: [LExpr] -> ModelVal
toModelVals = ManyVal . map toModelVal
