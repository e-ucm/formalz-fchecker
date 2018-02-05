module LogicIR.Backend.QuickCheck.ModelGenerator (generateModel, generateArrayModel, Model, ArrayModel) where

import Data.List (find, nub, (\\))
import System.Random
import Data.Maybe (isNothing, fromJust)
import Data.Map (Map)
import qualified Data.Map.Lazy as M

import LogicIR.Expr

type Model = [(LExpr, LExpr)]
type ArrayModel = Map Var [LConst]

-- Generates a constant value for all vars vars in the given list.
generateModel :: [LExpr] -> IO Model
generateModel = mapM generateModelEntry

generateArrayModel :: [LExpr] -> IO ArrayModel
generateArrayModel es = mapM generateArray es >>= \l -> return (M.fromList l)
    where generateArray e@(LVar v@(Var (TArray (TPrim t)) _)) = do
              len         <- generateIntWithBounds (0,4)
              let b        = bounds t
              expArr      <- mapM (\x -> generatePrimitiveWithBounds t b) [1..len]
              let cnstArr  = map (\(LConst c) -> c) expArr
              return (v, cnstArr)

{- Generates an entry for the substitution Model for a given LExpr.

   Note that firstly:  LIsnull can only be applied to arrays since ints/bools cannot be null in Java,
             secondly: LIsnull is not substituted based on the actual value of the array in the
                       ArrayModel. Instead, it's just a random bool that is present in the primitives model.
                       This is because otherwise, the evaluation of an LExpr should be changed drastically.
                       After all, How do you evaluate a[0] or a.length if a == null... -}
generateModelEntry :: LExpr -> IO (LExpr, LExpr)
generateModelEntry e@(LVar (Var (TPrim t) _)) =
    generatePrimitive t >>= \v -> return (e, v)
generateModelEntry e@(LIsnull (Var (TArray _) _)) =
    generatePrimitive PBool >>= \v -> return (e, v)
generateModelEntry e =
    error $ "Cannot generate model entry for " ++ show e

generatePrimitive :: Primitive -> IO LExpr
generatePrimitive t = generatePrimitiveWithBounds t (bounds t)

-- Generates a random LExpr of a certain Primitive type.
generatePrimitiveWithBounds :: Primitive -> (Int, Int) -> IO LExpr
generatePrimitiveWithBounds t b = do
    v <- getStdRandom (randomR b)
    return $ toLExpr t v

generateIntWithBounds :: (Int, Int) -> IO Int
generateIntWithBounds b = getStdRandom (randomR b)

-- Returns the bounds within which a random value should be generated for
-- some type primitive type.
bounds :: Primitive -> (Int, Int)
bounds PBool  = (0, 1)
bounds PInt = (-10, 10)

-- Generates an LExpr given a Primitive type and a value.
toLExpr :: Primitive -> Int -> LExpr
toLExpr PBool  v = LConst $ CBool $ v == 1
toLExpr PInt v = LConst $ CInt v
