module WLP where

import Language.Java.Syntax
import Language.Java.Lexer
import Language.Java.Parser



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
wlpStmtAlgebra :: StmtAlgebra (Exp -> Exp)
wlpStmtAlgebra = (fStmtBlock, fIfThen, fIfThenElse, fWhile, fBasicFor, fEnhancedFor, fEmpty, fExpStmt, fAssert, fSwitch, fDo, fBreak, fContinue, fReturn, fSynchronized, fThrow, fTry, fLabeled) where
    fStmtBlock (Block bs)   = foldr ((.) . wlpBlock) id bs
    fIfThen e s1            = fIfThenElse e s1 id
    fIfThenElse e s1 s2     = (\q -> (e &* s1 q) |* (neg e &* s2 q))
    fWhile              = undefined
    fBasicFor           = undefined
    fEnhancedFor        = undefined
    fEmpty              = id
    fExpStmt            = undefined
    fAssert             = undefined
    fSwitch             = undefined
    fDo                 = undefined
    fBreak              = undefined
    fContinue           = undefined
    fReturn             = undefined
    fSynchronized       = undefined
    fThrow              = undefined
    fTry                = undefined
    fLabeled            = undefined
    -- Helper functions
    wlpBlock b = case b of
                     BlockStmt s            -> wlp s
                     LocalClass _           -> id
                     LocalVars mods t vars  -> undefined
    
    
-- | Calculates the weakest liberal pre-condition of a statement and a given post-condition
wlp :: Stmt -> Exp -> Exp
wlp = foldStmt wlpStmtAlgebra

