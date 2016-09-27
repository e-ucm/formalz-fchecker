module WLP where

import Language.Java.Syntax
import Language.Java.Lexer
import Language.Java.Parser
import Data.Maybe

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
    

-- Some constants:

-- Differentiate between different exceptions?
diffExc :: Bool
diffExc = False

true :: Exp
true = Lit (Boolean True)

false :: Exp
false = Lit (Boolean False)
    
    
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
type Inh = (Exp -> Exp, -- The accumulated transformer of the current block up until the current statement
            Exp -> Exp, -- The accumulated transformer up until the last loop (this is used when handling break statements etc.)
            Exp -> Exp, -- The wlp of the current loop statement not including initialization code. It refers to the loop starting from the loop continuation point.
            Maybe ([Catch], Bool) -- The catches when executing a block in a try statement, and a Bool indicating wether there is a finally-block
            )
            
-- | A type synonym for the synthesized attribute
type Syn = Exp -> Exp
    
-- | The algebra that defines the wlp transformer for statements
--   The synthesized attribute is the resulting transformer. 
--   Statements that pass control to the next statement have to explicitly combine their wlp function with the accumulated function, as some statements (e.g. break) ignore the accumulated function.
wlpStmtAlgebra :: StmtAlgebra (Inh -> Syn)
wlpStmtAlgebra = (fStmtBlock, fIfThen, fIfThenElse, fWhile, fBasicFor, fEnhancedFor, fEmpty, fExpStmt, fAssert, fSwitch, fDo, fBreak, fContinue, fReturn, fSynchronized, fThrow, fTry, fLabeled) where
    fStmtBlock (Block bs) (acc, br, loop, catch)        = foldr (\b r -> wlpBlock (r, br, loop, catch) b) acc bs -- The result of the last block-statement will be the accumulated transformer for the second-last etc.
    fIfThen e s1                                        = fIfThenElse e s1 (const id) -- if-then is just an if-then-else with an empty else-block
    fIfThenElse e s1 s2 inh@(acc, _, _, _)              = (\q -> (e &* s1 inh q) |* (neg e &* s2 inh q)) . acc
    fWhile e s (acc, br, _, catch)                      = let loop = (\q -> if isTrue (((inv &* neg e) `imp` q) &* ((inv &* e) `imp` s (id, br, loop, catch) inv)) then inv else (neg e &* q)) in loop . acc
    fBasicFor init e incr s inh@(acc, br, loop, catch)  = let loop' = fWhile (fromMaybeGuard e) (\inh' -> s (wlp' inh' (incrToStmt incr), br, loop', catch)) inh in wlp' (loop', br, loop, catch) (initToStmt init) 
    fEnhancedFor                                        = error "TODO: EnhancedFor"
    fEmpty (acc, _, _, _)                               = acc -- Empty does nothing, but still passes control to the next statement
    fExpStmt                                            = error "TODO: ExpStmt"
    fAssert e _ inh@(acc, _, _, _)                      = (e &*) . acc
    fSwitch e bs inh@(acc, _, _, _)                     = let (e', s1, s2) = desugarSwitch e bs in fIfThenElse e' (flip wlp' s1) (flip wlp' s2) inh . acc
    fDo s e (acc, br, _, catch)                         = let loop = s (fWhile e s (acc, br, loop, catch), br, loop, catch) in loop -- Do is just a while with the statement block executed one additional time. Break and continue still have to be handled in this additional execution.
    fBreak _ (_, br, _, _)                              = br -- wlp of the breakpoint
    fContinue _ (_, _, loop, _)                         = loop -- wlp of the loop
    fReturn                                             = error "TODO: Return"
    fSynchronized _                                     = fStmtBlock
    fThrow e inh@(acc, br, loop, catch)                 = case catch of
                                                             Nothing      -> (\q -> q &* throwException e) -- acc is ignored, as the rest of the block is not executed
                                                             Just (cs, f) -> maybe (if f then id else (\q -> q &* throwException e)) (flip fStmtBlock (id, br, loop, Nothing)) (getCatch e cs)
    fTry b cs f inh@(acc, br, loop, catch)              = fStmtBlock b (id, br, loop, Just (cs, isJust f)) . maybe acc (flip fStmtBlock inh) f -- The finally-block is always executed
    fLabeled _ s                                        = s
    
    -- Helper functions
    wlpBlock :: Inh -> BlockStmt -> Syn
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
    
    -- Converts a switch into nested if-then-else statements. The switch is assumed to be non-trivial.
    desugarSwitch :: Exp -> [SwitchBlock] -> (Exp, Stmt, Stmt)
    desugarSwitch e [SwitchBlock l bs]       = case l of
                                                    SwitchCase e' -> (BinOp e Equal e', StmtBlock (Block bs), Empty)
                                                    Default -> (true, StmtBlock (Block bs), Empty)
    desugarSwitch e (SwitchBlock l bs:sbs)   = case l of
                                                    SwitchCase e' -> (BinOp e Equal e', StmtBlock (Block bs), sbscode)
                                                    Default -> (true, StmtBlock (Block bs), sbscode)
        where sbscode = let (e, s1, s2) = desugarSwitch e sbs in IfThenElse e s1 s2
        
    throwException :: Exp -> Exp
    throwException e = if diffExc then MethodInv (MethodCall (Name [Ident "Exception"]) [e]) else false
    
    getCatch :: Exp -> [Catch] -> Maybe Block
    getCatch e []             = Nothing
    getCatch e (Catch p b:cs) = if p `catches` e then Just b else getCatch e cs
    
    catches :: FormalParam -> Exp -> Bool
    catches (FormalParam _ t _ _) e = True -- TODO
    
-- | Calculates the weakest liberal pre-condition of a statement and a given post-condition
wlp :: Stmt -> Exp -> Exp
wlp = wlp' (id, id, id, Nothing)

-- wlp' lets you specify the inherited attributes
wlp' :: Inh -> Stmt -> Exp -> Exp
wlp' inh s = foldStmt wlpStmtAlgebra s inh

