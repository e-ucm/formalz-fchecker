{-# LANGUAGE OverloadedStrings #-}
module LogicIR.Normalizer where

import qualified Data.Map as M
import Data.List (sortOn)
import Control.Arrow ((>>>))

import JavaHelpers.Folds
import Language.Java.Syntax
import Language.Java.Pretty

import LogicIR.Expr hiding (var, binOp)
import LogicIR.Fold
import LogicIR.Parser ()
import LogicIR.TypeChecker (typeOf)

--------------------------
-- Rewriting combinators.

-- | A rewriting function.
type Rewrite a = a -> a

-- | Compose two transformations (up to fixpoint).
(>>*) :: Eq a => Rewrite a -> Rewrite a -> Rewrite a
f >>* g = fixpoint f >>> fixpoint g

-- | Converge to fixpoint with given initial value.
fixpoint :: Eq a => Rewrite a -> Rewrite a
fixpoint k l =
  if l == l' then l
             else fixpoint k l'
  where l' = k l

--------------------------
-- Java

-- | Pattern synonym for single-parameter lambdas.
pattern Lam :: Ident -> Exp -> Exp
pattern Lam x e =
  Lambda (LambdaSingleParam x) (LambdaExpression e)

-- | Pattern synonym for single-argument DSL functions.
-- i.e. with, forall, exists
pattern Fun :: String -> Argument -> Ident -> Exp -> Exp
pattern Fun f x xVar e =
  MethodInv (MethodCall (Name [Ident f]) [x, Lam xVar e])

-- | Pattern synonym for three-argument DSL functions.
-- i.e. forallr, exists
pattern Fun3 :: String -> Argument -> Exp -> Exp -> Ident -> Exp -> Exp
pattern Fun3 f x begin end xVar e =
  MethodInv (MethodCall (Name [Ident f]) [x, begin, end, Lam xVar e])

-- | Pattern synonym for `with` statements.
pattern With :: Argument -> Ident -> Exp -> Exp
pattern With x xVar e =
  MethodInv (MethodCall (Name [Ident "with"]) [x, Lam xVar e])

-- | Pattern synonym for `imp` statements.
pattern Imp :: Exp -> Exp -> Exp
pattern Imp e e' =
  MethodInv (MethodCall (Name [Ident "imp"]) [e, e'])

-- | Find all "complex" expressions (i.e. index/len/null of non-variable expressions)
-- and introduce a `with` statement to replace the complex expression with a
-- new dummy variable (named by pretty-printing the expression).
introduceWiths :: Rewrite Exp
introduceWiths = foldExp $
  defExpAlgebraExp { arrayAccess = fArrayAccess, fieldAccess = fFieldAccess
                   , lambda = fLambda, methodInv = fMethodInv, binOp = fBinOp }
  where
    refold = fixpoint introduceWiths

    fArrayAccess i
      | ArrayIndex x es <- i
      , isNonTrivial x
      = let xVar = Ident $ "$" ++ prettyPrint x
        in  With x xVar (ArrayAccess $ ArrayIndex (ExpName $ Name [xVar]) es)

      | ArrayIndex ar (iExp : es) <- i
      , not (null es)
      = let x = ArrayAccess $ ArrayIndex ar [iExp]
            xVar = Ident $ "$" ++ prettyPrint x
        in  With x xVar (ArrayAccess $ ArrayIndex (ExpName $ Name [xVar]) es)

      | otherwise = ArrayAccess i

    fFieldAccess facc
      | PrimaryFieldAccess e (Ident "length") <- facc
      , isNonTrivial e
      = let eVar = Ident $ "$" ++ prettyPrint e
        in  With e eVar (ExpName $ Name [eVar, Ident "length"])

      | otherwise = FieldAccess facc

    fBinOp e1 op e2
      | op `elem` [Equal, NotEq]
      , isNonTrivial e1
      , e2 == Lit Null
      = let eVar = Ident $ "$" ++ prettyPrint e1
        in  With e1 eVar $ BinOp (ExpName $ Name [eVar]) op e2

      | op `elem` [Equal, NotEq]
      , e1 == Lit Null
      , isNonTrivial e2
      = let eVar = Ident $ "$" ++ prettyPrint e1
        in  With e2 eVar $ BinOp e1 op (ExpName $ Name [eVar])

      | otherwise
      = BinOp e1 op e2

    fLambda xs es
      | Lam x e <- Lambda xs es
      = Lam x (refold e)

      | otherwise
      = error $ "Unsupported λ-expression: " ++ prettyPrint (Lambda xs es)

    fMethodInv inv
      | Fun f x yVar e <- MethodInv inv
      , f `elem` ["forall", "exists"]
      , isNonTrivial x
      = let xVar = Ident $ "$" ++ prettyPrint x
        in  With x xVar (Fun f (ExpName $ Name [xVar]) yVar e)

      | Fun3 f x begin end yVar e <- MethodInv inv
      , f `elem` ["forallr", "existsr"]
      , isNonTrivial x
      = let xVar = Ident $ "$" ++ prettyPrint x
        in  With x xVar (Fun3 f (ExpName $ Name [xVar]) begin end yVar e)

      | Fun f x xVar e <- MethodInv inv
      = Fun f (refold x) xVar (refold e)

      | Fun3 f x begin end xVar e <- MethodInv inv
      = Fun3 f (refold x) (refold begin) (refold end) xVar (refold e)

      | Imp e e' <- MethodInv inv
      = Imp (refold e) (refold e')

      | otherwise
      = error $ "Unsupported method invocation: " ++ prettyPrint inv

-- | Bring the introduced `with` statements to a valid scope, such as the
-- resulting expression type-checks.
fixWiths :: Rewrite Exp
fixWiths = foldExp $
  defExpAlgebraExp { binOp = fBinOp, preNot = fPreNot, prePlus = fPrePlus
                   , preMinus = fPreMinus, methodInv = fMethodInv
                   , lambda = fLambda }
  where
    refold = fixpoint fixWiths

    -- Always move `with` statements out of arithmetic/logical expressions.
    fPrePlus e
      | With x xVar e' <- e
      = With x xVar (PrePlus e')

      | otherwise
      = PrePlus e

    fPreMinus e
      | With x xVar e' <- e
      = With x xVar (PreMinus e')

      | otherwise
      = PreMinus e

    fPreNot e
      | With x xVar e' <- e
      = With x xVar (PreNot e')

      | otherwise
      = PreNot e

    fBinOp e1 op e2
      | With x xVar e1' <- e1
      = With x xVar $ BinOp e1' op e2

      | With x xVar e2' <- e2
      = With x xVar $ BinOp e1 op e2'

      | otherwise
      = BinOp e1 op e2

    fMethodInv inv
      -- Optimize multiple `withs` that hold the same expression
      | With x xVar (With y yVar e) <- MethodInv inv
      , x == y
      = With x xVar (substExp e $ M.singleton yVar xVar)

      -- Sort consecutive withs, so as to have a canonical form.
      | With x xVar (With y yVar e) <- MethodInv inv
      , x /= y
      , xVar /= yVar
      , [(x', xVar'), (y', yVar')] <- sortOn (show . fst) [(x, xVar), (y, yVar)]
      = With x' xVar' $ With y' yVar' e

      -- A `with` statement can only escape a single-argument lambda expression
      -- (i.e. with/forall/exists), if it does not refer to the bound variable .
      | Fun f x xVar (With y yVar e) <- MethodInv inv
      , xVar `notIn` y
      , xVar /= yVar
      = With y yVar $ Fun f x xVar e

      -- A `with` statement can only escape a three-argument lambda expression
      -- (i.e. forallr/existsr), if it does not refer to the bound variable.
      -- NB: these `withs` can appear in all three places (begin, end, body)
      | Fun3 f x (With y yVar begin) end xVar e <- MethodInv inv
      , xVar `notIn` y
      , xVar /= yVar
      = With y yVar $ Fun3 f x begin end xVar e

      | Fun3 f x begin (With y yVar end) xVar e <- MethodInv inv
      , xVar `notIn` y
      , xVar /= yVar
      = With y yVar $ Fun3 f x begin end xVar e

      | Fun3 f x begin end xVar (With y yVar e) <- MethodInv inv
      , xVar `notIn` y
      , xVar /= yVar
      = With y yVar $ Fun3 f x begin end xVar e

      -- A `with` statement always escapes implications (i.e. imp() calls).
      | Imp (With x xVar e) e' <- MethodInv inv
      = With x xVar $ Imp e e'

      | Imp e (With x xVar e') <- MethodInv inv
      = With x xVar $ Imp e e'

      -- Continue downwards...
      | MethodCall m es <- inv
      = MethodInv $ MethodCall m (refold <$> es)

      -- Unsupported segment of the Java language.
      | otherwise
      = error $ "Unsupported method invocation: " ++ prettyPrint inv

    -- Continue downwards...
    fLambda xs es
      -- only support single-argument lambdas
      | Lam x e <- Lambda xs es
      = Lam x (refold e)

      | otherwise
      = error $ "Unsupported λ-expression: " ++ prettyPrint (Lambda xs es)

-- | Simple rewrites to simplify a logical expression in our DSL.
simplify :: Rewrite Exp
simplify = foldExp $
  defExpAlgebraExp { preNot = fPreNot, methodInv = fMethodInv, lambda = fLambda }
  where
    refold = fixpoint simplify

    fPreNot e
      -- Binary operators
      | BinOp e1 LThan e2 <- e
      = BinOp e1 GThanE e2

      | BinOp e1 GThan e2 <- e
      = BinOp e1 LThanE e2

      | BinOp e1 LThanE e2 <- e
      = BinOp e1 GThan e2

      | BinOp e1 GThanE e2 <- e
      = BinOp e1 LThan e2

      | BinOp e1 GThanE e2 <- e
      = BinOp e1 LThan e2

      | BinOp e1 Equal e2 <- e
      = BinOp e1 NotEq e2

      | BinOp e1 And e2 <- e
      = BinOp (PreNot e1) Or (PreNot e2)

      | BinOp e1 Or e2 <- e
      = BinOp (PreNot e1) And (PreNot e2)

      -- Quantifiers
      | Fun "forall" x yVar e' <- e
      = Fun "exists" x yVar (PreNot e')

      | Fun "exists" x yVar e' <- e
      = Fun "forall" x yVar (PreNot e')

      -- Ranged quantifiers
      | Fun3 "existsr" x begin end yVar e' <- e
      = Fun3 "forallr" x begin end yVar (PreNot e')

      | Fun3 "forallr" x begin end yVar e' <- e
      = Fun3 "existsr" x begin end yVar (PreNot e')

      -- With expressions
      | With x xVar e' <- e
      = With x xVar (PreNot e')

      -- Continue downwards...
      | otherwise
      = PreNot (refold e)

    -- Continue downwards...
    fMethodInv inv
      | MethodCall m es <- inv
      = MethodInv $ MethodCall m (refold <$> es)

      | otherwise
      = error $ "Unsupported method invocation: " ++ prettyPrint inv

    -- Continue downwards...
    fLambda xs es
      -- only support single-argument lambdas
      | Lam x e <- Lambda xs es
      = Lam x (refold e)

      | otherwise
      = error $ "Unsupported λ-expression: " ++ prettyPrint (Lambda xs es)

-- | A substitution maps elements to their substitution (preserving the type).
type Substitution a = M.Map a a

-- | Substitute a variable for another over a Java expression.
substExp :: Exp -> Substitution Ident -> Exp
substExp e substMap =
  let sb ex = substExp ex substMap
  in case e of
    Lit l              -> Lit l
    PrePlus e'         -> PrePlus (sb e')
    PreMinus e'        -> PreMinus (sb e')
    PreNot e'          -> PreNot (sb e')
    BinOp e1 op e2     -> BinOp (sb e1) op (sb e2)
    Cond e1 e2 e3      -> Cond (sb e1) (sb e2) (sb e3)
    ExpName (Name xs) ->
      ExpName $ Name $ (\x -> M.findWithDefault x x substMap) <$> xs
    MethodInv (MethodCall f args) ->
      MethodInv $ MethodCall f (sb <$> args)
    ArrayAccess (ArrayIndex eArr eIs) ->
      ArrayAccess $ ArrayIndex (sb eArr) (sb <$> eIs)
    Lam x e' ->
      Lam x (substExp e' $ M.delete x substMap)
    _ -> error $ "substExp: not supported (" ++ prettyPrint e ++ ")"

-- | Check whether a variable *does not* in a Java expression.
notIn :: Ident -> Exp -> Bool
notIn x e = e == substExp e (M.singleton x (Ident "$$"))

-- | Check whether a Java expression is not trivial.
isNonTrivial :: Exp -> Bool
isNonTrivial (ExpName (Name [_])) = False
isNonTrivial (Lit _)              = False
isNonTrivial _                    = True

--------------------------
-- LogicIR [ON_HOLD]
--
-- * TODO constant folding, etc...

-- | Convert a logic formula to Conjunctive Normal Form (CNF).
toCNF :: Rewrite LExpr
toCNF = fixpoint (toNNF >>* skolemize >>* distribute >>* domainCNF)

-- | Convert a logic formula to Negation Normal Form (NNF).
toNNF :: Rewrite LExpr
toNNF = elimEquiv >>* elimImpl >>* notInwardsL
  where
    elimEquiv :: Rewrite LExpr
    elimEquiv = foldLExpr (defLAlgebra {bin = fBin})
      where fBin x op y
              | op == CEqual && typeOf x == Right "bool" =
                  (x .==> y) .&& (y .==> x)
              | otherwise = LBinop x op y
    elimImpl :: Rewrite LExpr
    elimImpl = foldLExpr (defLAlgebra {bin = fBin})
      where fBin x LImpl y = lnot x .|| y
            fBin x o y     = LBinop x o y
    notInwardsL :: Rewrite LExpr
    notInwardsL = foldLExpr (defLAlgebra {uni = fUni})
      where fUni LNot (LBinop x LOr y)    = lnot x .&& lnot y
            fUni LNot (LBinop x LAnd y)   = lnot x .|| lnot y
            fUni LNot (LUnop LNot y)      = y
            fUni LNot (LQuant QAll x d a) = exists x d (lnot a)
            fUni LNot (LQuant QAny x d a) = forall x d (lnot a)
            fUni o x                      = LUnop o x

-- | Quantifier elimination.
skolemize :: Rewrite LExpr
skolemize = foldLExpr (defLAlgebra {bin = fBin})
  where fBin p LAnd (LQuant o x d a) = LQuant o x d (p .&& a)
        fBin p LOr  (LQuant o x d a) = LQuant o x d (p .|| a)
        fBin x o y                   = LBinop x o y

-- | Rewrite the rule `a \/ (b /\ c) ~> (a \/ b) /\ (a \/ c)`.
distribute :: Rewrite LExpr
distribute = foldLExpr (defLAlgebra {bin = fBin})
  where fBin p LOr (LBinop x LAnd y) = (p .|| x) .&& (p .|| y)
        fBin x o y                   = LBinop x o y

domainCNF :: Rewrite LExpr
domainCNF = foldLExpr (defLAlgebra {qnt = fQnt})
  where fQnt o x d = LQuant o x (toCNF d)
