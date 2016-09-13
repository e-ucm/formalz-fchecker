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
                  BasicFor init e es stmt -> fBasicFor init e es (fold stmt)
                  _ -> error "TODO: complete foldStmt cases"
    
-- | The algebra that defines the wlp transformer for statements
--   The synthesized attribute is the resulting transformer. 
--   The inherited attribute represent the accumulated transformer up till the last block (this is used when handling break statements etc.)
wlpStmtAlgebra :: StmtAlgebra ((Exp -> Exp) -> (Exp -> Exp))
wlpStmtAlgebra = (fStmtBlock, fIfThen, fIfThenElse, fWhile, fBasicFor, fEnhancedFor, fEmpty, fExpStmt, fAssert, fSwitch, fDo, fBreak, fContinue, fReturn, fSynchronized, fThrow, fTry, fLabeled) where
    fStmtBlock (Block bs) inh   = let x = foldr ((.) . wlpBlock x) id bs in x
    fIfThen e s1                = fIfThenElse e s1 (const id)
    fIfThenElse e s1 s2 inh     = (\q -> (e &* (s1 inh) q) |* (neg e &* (s2 inh) q))
    fWhile e s inh              = (\q -> if isTrue ((inv &* neg e) `imp` q) then inv else (neg e &* q)) -- We assume the given invariant is correct
    fBasicFor           = undefined
    fEnhancedFor        = undefined
    fEmpty inh                  = id
    fExpStmt            = undefined
    fAssert             = undefined
    fSwitch             = undefined
    fDo                 = undefined
    fBreak _ inh                = inh
    fContinue           = undefined
    fReturn             = undefined
    fSynchronized       = undefined
    fThrow              = undefined
    fTry                = undefined
    fLabeled            = undefined
    -- Helper functions
    wlpBlock inh b = case b of
                        BlockStmt s            -> wlp' inh s
                        LocalClass _           -> id
                        LocalVars mods t vars  -> undefined
    inv = Lit (Boolean True) -- for simplicity, "True" is used as an invariant for now
    
    
-- | Calculates the weakest liberal pre-condition of a statement and a given post-condition
wlp :: Stmt -> Exp -> Exp
wlp = wlp' id

-- wlp' lets you specify the inherited attribute
wlp' :: (Exp -> Exp) -> Stmt -> Exp -> Exp
wlp' inh s = foldStmt wlpStmtAlgebra s inh

