{-# LANGUAGE ScopedTypeVariables #-}

module LogicIR.Backend.QuickCheck.ModelGenerator (generateModel, generateArrayModel, generateEvalInfo, expandEvalInfo, findVariables, Model, ArrayModel, EvalInfo) where

import Data.List (find, nub, (\\))
import System.Random
import Data.Maybe (isNothing, fromJust)
import Data.Map (Map)
import qualified Data.Map.Lazy as M

import LogicIR.Expr
import LogicIR.Fold

type Model = [(LExpr, LExpr)]
type ArrayModel = Map Var [LConst]

-- EvalInfo contains all information needed to evaluate an LExpr.
-- The third tuple element is a list of all Quantifier iterator vars.
type EvalInfo = (Model, ArrayModel, [LExpr])

-- If a model was generated for e1, then it is possible that there are
-- still vars in e2 that do not appear in e1 and therefore not in its
-- model. This is where expandModel comes into play.
expandEvalInfo :: LExpr -> (Model, ArrayModel) -> IO EvalInfo
expandEvalInfo e (m, arrM) = do
    (m', arrM', quantVs') <- generateEvalInfo e
    let mFinal    = m    ++ filter (\(k,v) -> not $ elem k (fst $ unzip m)) m'
    let arrMFinal = (M.toList arrM) ++ filter (\(k,v) -> not $ elem k (M.keys arrM)) (M.toList arrM')
    return (mFinal, M.fromList arrMFinal, quantVs')

generateEvalInfo :: LExpr -> IO EvalInfo
generateEvalInfo e = do
    let vs@(regularVs, (arrayVs, arrayOpVs), quantVs) = findVariables e
    let isNullExprs   = filter sIsnullExpr arrayOpVs
    primitivesModel  <- generateModel (regularVs ++ isNullExprs)
    arrayModel       <- generateArrayModel arrayVs
    return (primitivesModel, arrayModel, quantVs)
    where sIsnullExpr (LIsnull _) = True
          sIsnullExpr _           = False

-- Generates a constant value for all vars vars in the given list.
generateModel :: [LExpr] -> IO Model
generateModel = mapM generateModelEntry

generateArrayModel :: [LExpr] -> IO ArrayModel
generateArrayModel es = mapM generateArray es >>= \l -> return (M.fromList l)
    where generateArray e@(LVar v@(Var (TArray (TPrim t)) _)) = do
              len         <- (rnd (0,4)) :: IO Int
              expArr      <- mapM (\x -> generatePrimitive t) [1..len]
              let cnstArr  = map (\(LConst c) -> c) expArr
              return (v, cnstArr)

{- Generates an entry for the substitution Model for a given LExpr.
   Note that firstly:  LIsnull can only be applied to arrays since ints/bools
                       cannot be null in Java,
             secondly: LIsnull is not substituted based on the actual value of
                       the array in the
                       ArrayModel. Instead, it's just a random bool that is
                       present in the primitives model. This is because
                       otherwise, the evaluation of an LExpr should be changed
                       drastically. After all, How do you evaluate a[0] or
                       a.length if a == null... -}
generateModelEntry :: LExpr -> IO (LExpr, LExpr)
generateModelEntry e@(LVar (Var (TPrim t) _)) =
    generatePrimitive t >>= \v -> return (e, v)
generateModelEntry e@(LIsnull (Var (TArray _) _)) =
    generatePrimitive PBool >>= \v -> return (e, v)
generateModelEntry e =
    error $ "Cannot generate model entry for " ++ show e

generatePrimitive :: Primitive -> IO LExpr
generatePrimitive t@PBool = do
    let b = (0,1) :: (Int,Int)
    v <- rnd b
    return $ LConst $ CBool $ v == 1
generatePrimitive t@PInt = do
    v <- rnd (-10,10)
    return $ LConst $ CInt v
generatePrimitive t@PReal = do
    v <- rnd (-500,500)
    let finalV = (v / 50) :: Double
    return $ LConst $ CReal finalV

rnd :: System.Random.Random a => (a, a) -> IO a
rnd b = getStdRandom (randomR b)

-- The first  list contains all regular var LExpr's.
-- The second item contains a list of all arrays and a list of array operators (isnull / len).
-- The third  list contains all quantifier indexer vars.
type VariableCollection = ([LExpr],([LExpr],[LExpr]),[LExpr])

findVariables :: LExpr -> VariableCollection
findVariables e = (a \\ c, b, c)
    where merge :: VariableCollection -> VariableCollection -> VariableCollection
          merge (a1,(b1,c1),d1) (a2,(b2,c2),d2) = (a1++a2,(b1++b2,c1++c2),d1++d2)

          algebra :: LExprAlgebra VariableCollection
          algebra = (cnst, var, uni, bin, iff, quant, arr, snull, len)
          cnst _           = ([],([],[]),[])
          uni _ e          = e
          bin le _ re      = merge le re
          iff ge e1 e2     = merge ge $ merge e1 e2
          var v            = ([LVar v],([],[]),[])
          quant qt v e1 e2 = merge ([],([],[]),[LVar v]) $ merge e1 e2
          arr v e          = merge ([],([LVar v],[]),[]) e
          snull v          = ([],([LVar v],[LIsnull v]),[])
          len v            = ([],([LVar v],[LLen v]),[])

          (afull,(bfull,cfull),dfull) = foldLExpr algebra e
          (a,b,c) = (nub afull,(nub bfull,nub cfull),nub dfull)

