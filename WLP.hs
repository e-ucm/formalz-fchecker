module WLP where

import Language.Java.Syntax
import Language.Java.Lexer
import Language.Java.Parser

import Verifier



type StmtAlgebra r = (Block -> r,           
                      Exp -> r -> r,        
                      Exp -> r -> r -> r,
                      Exp -> r -> r,
                      (Maybe ForInit) -> (Maybe Exp) -> (Maybe [Exp]) -> r -> r,
                      [Modifier] -> Type -> Ident -> Exp -> r -> r,
                      r, 
                      Exp -> r,
                      Exp -> (Maybe Exp) -> r,
                      Exp -> [SwitchBlock] -> r,
                      r -> Exp -> r,
                      Maybe Ident -> r,
                      Maybe Ident -> r,
                      Maybe Exp -> r, 
                      Exp -> Block -> r,
                      Exp -> r,
                      Block -> [Catch] -> (Maybe Block) -> r,
                      Ident -> r -> r
                      )
    

-- Logical operators for expressions:
(&*) :: Exp -> Exp -> Exp
e1 &* e2 = BinOp e1 And e2

(|*) :: Exp -> Exp -> Exp
e1 |* e2 = BinOp e1 Or e2

neg :: Exp -> Exp
neg = PreNot

imp :: Exp -> Exp -> Exp
e1 `imp` e2 =  (e1 &* e2) |* neg e1



    
-- | A fold function over a java statement.
foldStmt :: StmtAlgebra r -> Stmt -> r
foldStmt (fStmtBlock, fIfThen, fIfThenElse, fWhile, fBasicFor, fEnhancedFor, fEmpty, fExpStmt, fAssert, fSwitch, fDo, fBreak, fContinue, fReturn, fSynchronized, fThrow, fTry, fLabeled) s = fold s where
    fold s = case s of
                  StmtBlock b -> fStmtBlock b
                  IfThen e stmt -> fIfThen e (fold stmt)	
                  IfThenElse e stmt1 stmt2 -> fIfThenElse e (fold stmt1) (fold stmt2)
                  While e stmt -> fWhile e (fold stmt)
                  BasicFor init e incr stmt -> fBasicFor init e incr (fold stmt)
                  _ -> error "TODO: complete foldStmt cases"
    

-- | A type synonym for the inherited attribute


type Inh = (Exp -> Exp, -- The accumulated transformer up till the last loop (this is used when handling break statements etc.)
            Exp -> Exp  -- The wlp of the current loop statement not including initialization code. It refers to the loop starting from the loop continuation point.
            )
    
-- | The algebra that defines the wlp transformer for statements
--   The synthesized attribute is the resulting transformer. 
wlpStmtAlgebra :: StmtAlgebra (Inh -> Exp -> Exp)
wlpStmtAlgebra = (fStmtBlock, fIfThen, fIfThenElse, fWhile, fBasicFor, fEnhancedFor, fEmpty, fExpStmt, fAssert, fSwitch, fDo, fBreak, fContinue, fReturn, fSynchronized, fThrow, fTry, fLabeled) where
    fStmtBlock (Block bs) inh               = foldr ((.) . wlpBlock inh) id bs
    fIfThen e s1                            = fIfThenElse e s1 (const id)
    fIfThenElse e s1 s2 inh                 = (\q -> (e &* s1 inh q) |* (neg e &* s2 inh q))
    fWhile e s (acc, _)                     = let loop = (\q -> if isTrue (((inv &* neg e) `imp` q) &* ((inv &* e) `imp` s (acc, loop) inv)) then inv else (neg e &* q)) in loop
    fBasicFor init e incr s inh@(acc, _)    = wlp' inh (initToStmt init) . let loop = fWhile (fromMaybeGuard e) (\inh' -> s (acc, loop) . wlp' inh' (incrToStmt incr)) inh in loop
    fEnhancedFor                            = error "TODO: EnhancedFor"
    fEmpty inh                              = id
    fExpStmt            = undefined
    fAssert             = undefined
    fSwitch             = undefined
    fDo                 = undefined
    fBreak _ (acc, _)                       = acc
    fContinue _ (acc, loop)                 = loop . acc
    fReturn             = undefined
    fSynchronized       = undefined
    fThrow              = undefined
    fTry                = undefined
    fLabeled            = undefined
    
    -- Helper functions
    wlpBlock :: Inh -> BlockStmt -> Exp -> Exp
    wlpBlock inh b = case b of
                        BlockStmt s            -> wlp' inh s
                        LocalClass _           -> id
                        LocalVars mods t vars  -> error "TODO: LocalVars"
                        
    inv = true -- for simplicity, "True" is used as an invariant for now
    
    -- Converts initialization code of a for loop to a statement
    initToStmt :: Maybe ForInit -> Stmt
    initToStmt Nothing                      = Empty
    initToStmt (Just (ForInitExps es))      = StmtBlock (Block (map (BlockStmt . ExpStmt) es))
    initToStmt (Just (ForLocalVars _ _ _))  = error "TODO: ForLocalVars"
    
    -- Replaces an absent guard with "True"
    fromMaybeGuard :: Maybe Exp -> Exp
    fromMaybeGuard Nothing  = true
    fromMaybeGuard (Just e) = e
    
    -- Converts increment code of a for loop to a statement
    incrToStmt :: Maybe [Exp] -> Stmt
    incrToStmt Nothing   = Empty
    incrToStmt (Just es) = StmtBlock (Block (map (BlockStmt . ExpStmt) es))
    
true :: Exp
true = Lit (Boolean True)
    
-- | Calculates the weakest liberal pre-condition of a statement and a given post-condition
wlp :: Stmt -> Exp -> Exp
wlp = wlp' (id, id)

-- wlp' lets you specify the inherited attributes
wlp' :: Inh -> Stmt -> Exp -> Exp
wlp' inh s = foldStmt wlpStmtAlgebra s inh

