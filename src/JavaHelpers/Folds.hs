-- Copyright (c) 2017 Utrecht University
-- Author: Koen Wermer

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- Folds over Java data structures
module JavaHelpers.Folds where

import Language.Java.Syntax

type StmtAlgebra r = (Block -> r, -- StmtBlock
                      Exp -> r -> r, -- IfThen
                      Exp -> r -> r -> r, -- IfThenElse
                      Exp -> r -> r,  -- While
                      Maybe ForInit -> Maybe Exp -> Maybe [Exp] -> r -> r,  -- BasicFor
                      [Modifier] -> Type -> Ident -> Exp -> r -> r,  -- EnhancedFor
                      r, -- Empty
                      Exp -> r, -- ExpStmt
                      Exp -> Maybe Exp -> r, -- Assert
                      Exp -> [SwitchBlock] -> r, -- Switch
                      r -> Exp -> r, -- Do
                      Maybe Ident -> r, -- Break
                      Maybe Ident -> r, -- Continue
                      Maybe Exp -> r, -- Return
                      Exp -> Block -> r, -- Synchronized
                      Exp -> r, -- Throw
                      Block -> [Catch] -> Maybe Block -> r, -- Try
                      Ident -> r -> r -- Labeled
                      )

data ExpAlgebra r = ExpAlgebra
  { lit :: Literal -> r
  , classLit :: Maybe Type -> r
  , this :: r
  , thisClass :: Name -> r
  , instanceCreation :: [TypeArgument] -> TypeDeclSpecifier -> [Argument] -> Maybe ClassBody -> r
  , qualInstanceCreation :: r -> [TypeArgument] -> Ident -> [Argument] -> Maybe ClassBody -> r
  , arrayCreate :: Type -> [r] -> Int -> r
  , arrayCreateInit :: Type -> Int -> ArrayInit -> r
  , fieldAccess :: FieldAccess -> r
  , methodInv :: MethodInvocation -> r
  , arrayAccess :: ArrayIndex -> r
  , expName :: Name -> r
  , postIncrement :: r -> r
  , postDecrement :: r -> r
  , preIncrement :: r -> r
  , preDecrement :: r -> r
  , prePlus :: r -> r
  , preMinus :: r -> r
  , preBitCompl :: r -> r
  , preNot :: r -> r
  , cast :: Type -> r -> r
  , binOp :: r -> Op -> r -> r
  , instanceOf :: r -> RefType -> r
  , cond :: r -> r -> r -> r
  , assign :: Lhs -> AssignOp -> r -> r
  , lambda :: LambdaParams -> LambdaExpression -> r
  , methodRef :: Name -> Ident -> r
  }

defExpAlgebra :: ExpAlgebra a
defExpAlgebra = ExpAlgebra { lit = no "Lit"
                           , classLit = no "ClassLit"
                           , this = no "This"
                           , thisClass = no "ThisClass"
                           , instanceCreation = no "InstanceCreation"
                           , qualInstanceCreation = no "QualInstanceCreation"
                           , arrayCreate = no "ArrayCreate"
                           , arrayCreateInit = no "ArrayCreateInit"
                           , fieldAccess = no "FieldAccess"
                           , methodInv = no "MethodInv"
                           , arrayAccess = no "ArrayAccess"
                           , expName = no "ExpName"
                           , postIncrement = no "PostIncrement"
                           , postDecrement = no "PostDecrement"
                           , preIncrement = no "PreIncrement"
                           , preDecrement = no "PreDecrement"
                           , prePlus = no "PrePlus"
                           , preMinus = no "PreMinus"
                           , preBitCompl = no "PreBitCompl"
                           , preNot = no "PreNot"
                           , cast = no "Cast"
                           , binOp =  no "BinOp"
                           , instanceOf = no "InstanceOf"
                           , cond = no "Cond"
                           , assign = no "Assign"
                           , lambda = no "Lambda"
                           , methodRef = no "MethodRef"
                           }
  where no = error . ("[javaExpToLExp] " ++) . (++ " is not supported")

defExpAlgebraExp :: ExpAlgebra Exp
defExpAlgebraExp =
  ExpAlgebra Lit ClassLit This ThisClass InstanceCreation QualInstanceCreation
             ArrayCreate ArrayCreateInit FieldAccess MethodInv ArrayAccess
             ExpName PostIncrement PostDecrement PreIncrement PreDecrement
             PrePlus PreMinus PreBitCompl PreNot Cast BinOp InstanceOf Cond
             Assign Lambda MethodRef

-- | A fold function over a java statement.
foldStmt :: StmtAlgebra r -> Stmt -> r
foldStmt (fStmtBlock, fIfThen, fIfThenElse, fWhile, fBasicFor, fEnhancedFor, fEmpty, fExpStmt, fAssert, fSwitch, fDo, fBreak, fContinue, fReturn, fSynchronized, fThrow, fTry, fLabeled) = fold where
    fold s = case s of
                  StmtBlock b -> fStmtBlock b
                  IfThen e stmt -> fIfThen e (fold stmt)
                  IfThenElse e stmt1 stmt2 -> fIfThenElse e (fold stmt1) (fold stmt2)
                  While e stmt -> fWhile e (fold stmt)
                  BasicFor init e incr stmt -> fBasicFor init e incr (fold stmt)
                  EnhancedFor mods t i e stmt -> fEnhancedFor mods t i e (fold stmt)
                  Empty -> fEmpty
                  ExpStmt e -> fExpStmt e
                  Assert e me -> fAssert e me
                  Switch e bs -> fSwitch e bs
                  Do stmt e -> fDo (fold stmt) e
                  Break l -> fBreak l
                  Continue l -> fContinue l
                  Return me -> fReturn me
                  Synchronized e b -> fSynchronized e b
                  Throw e -> fThrow e
                  Try b cs f -> fTry b cs f
                  Labeled l stmt -> fLabeled l (fold stmt)

-- | A fold function over a java expression.
foldExp :: ExpAlgebra r -> Exp -> r
foldExp (ExpAlgebra fLit fClassLit fThis fThisClass fInstanceCreation fQualInstanceCreation
                    fArrayCreate fArrayCreateInit fFieldAccess fMethodInv fArrayAccess
                    fExpName fPostIncrement fPostDecrement fPreIncrement fPreDecrement
                    fPrePlus fPreMinus fPreBitCompl fPreNot fCast fBinOp fInstanceOf
                    fCond fAssign fLambda fMethodRef) = fold
  where
    fold e = case e of
                  Lit lit -> fLit lit
                  ClassLit mt -> fClassLit mt
                  This -> fThis
                  ThisClass name -> fThisClass name
                  InstanceCreation typeArgs classType args mBody -> fInstanceCreation typeArgs classType args mBody
                  QualInstanceCreation e typeArgs ident args mBody -> fQualInstanceCreation (fold e) typeArgs ident args mBody
                  ArrayCreate t exps dim -> fArrayCreate t (map fold exps) dim
                  ArrayCreateInit t dim arrayInit -> fArrayCreateInit t dim arrayInit
                  FieldAccess fieldAccess -> fFieldAccess fieldAccess
                  MethodInv invocation -> fMethodInv invocation
                  ArrayAccess i -> fArrayAccess i
                  ExpName name -> fExpName name
                  PostIncrement e -> fPostIncrement (fold e)
                  PostDecrement  e -> fPostDecrement  (fold e)
                  PreIncrement e -> fPreIncrement (fold e)
                  PreDecrement  e -> fPreDecrement  (fold e)
                  PrePlus e -> fPrePlus (fold e)
                  PreMinus e -> fPreMinus (fold e)
                  PreBitCompl e -> fPreBitCompl (fold e)
                  PreNot e -> fPreNot (fold e)
                  Cast t e -> fCast t (fold e)
                  BinOp e1 op e2 -> fBinOp (fold e1) op (fold e2)
                  InstanceOf e refType -> fInstanceOf (fold e) refType
                  Cond g e1 e2 -> fCond (fold g) (fold e1) (fold e2)
                  Assign lhs assOp e -> fAssign lhs assOp (fold e)
                  Lambda lParams lExp -> fLambda lParams lExp
                  MethodRef className methodName -> fMethodRef className methodName
