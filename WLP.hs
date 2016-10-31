module WLP where

import Language.Java.Syntax
import Language.Java.Lexer
import Language.Java.Parser
import Data.Maybe
import Data.List

import Folds
import Verifier
import Substitute
import HelperFunctions


-- Settings:

-- Differentiate between different exceptions?
diffExc :: Bool
diffExc = False
    

-- | A type synonym for the inherited attribute
type Inh = (Exp -> Exp, -- The accumulated transformer of the current block up until the current statement
            Exp -> Exp, -- The accumulated transformer up until the last loop (this is used when handling break statements etc.)
            Exp -> Exp, -- The wlp of the current loop statement not including initialization code. It refers to the loop starting from the loop continuation point.
            Maybe ([Catch], Bool), -- The catches when executing a block in a try statement, and a Bool indicating wether there is a finally-block
            TypeEnv) -- The type environment for typing expressions
            
           
-- | A type synonym for the synthesized attributea
type Syn = (Exp -> Exp, -- The wlp transformer
            TypeEnv)    -- The type environment

type TypeEnv = [(Name, Type)]
    
-- | The algebra that defines the wlp transformer for statements
--   The synthesized attribute is the resulting transformer. 
--   Statements that pass control to the next statement have to explicitly combine their wlp function with the accumulated function, as some statements (e.g. break) ignore the accumulated function.
wlpStmtAlgebra :: StmtAlgebra (Inh -> Syn)
wlpStmtAlgebra = (fStmtBlock, fIfThen, fIfThenElse, fWhile, fBasicFor, fEnhancedFor, fEmpty, fExpStmt, fAssert, fSwitch, fDo, fBreak, fContinue, fReturn, fSynchronized, fThrow, fTry, fLabeled) where
    fStmtBlock (Block bs) (acc, br, loop, catch, env)       = foldr (\b (r, env') -> wlpBlock (r, br, loop, catch, env') b) (acc, envBlock bs env) bs -- The result of the last block-statement will be the accumulated transformer for the second-last etc. The type environment is build from the left, so it has to be done seperately.
    fIfThen e s1                                            = fIfThenElse e s1 (const (id, [])) -- if-then is just an if-then-else with an empty else-block
    fIfThenElse e s1 s2 inh@(acc, _, _, _, env)             = ((\q -> (e &* fst (s1 inh) q) |* (neg e &* fst (s2 inh) q)) . acc, env)
    fWhile e s (acc, br, _, catch, env)                     = let loop = (\q -> if unsafeIsTrue (((inv &* neg e) `imp` q) &* ((inv &* e) `imp` fst (s (id, br, loop, catch, env)) inv)) then inv else (neg e &* q)) in (loop . acc, env)
    fBasicFor init me incr s inh@(acc, br, loop, catch, env) = let loop' = fst (fWhile (fromMaybeGuard me) (\inh' -> s (fst (wlp' inh' (incrToStmt incr)), br, loop', catch, env)) inh) in wlp' (loop', br, loop, catch, env) (initToStmt init) 
    fEnhancedFor                                            = error "TODO: EnhancedFor"
    fEmpty (acc, _, _, _, env)                              = (acc, env) -- Empty does nothing, but still passes control to the next statement
    fExpStmt e inh                                          = snd $ foldExp wlpExpAlgebra e inh
    fAssert e _ inh@(acc, _, _, _, env)                     = ((e &*) . acc, env)
    fSwitch e bs inh@(acc, _, _, _, env)                    = let (e', s1, s2) = desugarSwitch e bs in fIfThenElse e' (flip wlp' s1) (flip wlp' s2) inh
    fDo s e (acc, br, _, catch, env)                        = let loop = fst (s (fst (fWhile e s (acc, br, loop, catch, env)), br, loop, catch, env)) in (loop, env) -- Do is just a while with the statement block executed one additional time. Break and continue still have to be handled in this additional execution.
    fBreak _ (_, br, _, _, env)                             = (br, env) -- wlp of the breakpoint
    fContinue _ (_, _, loop, _, env)                        = (loop, env) -- wlp of the loop
    fReturn me inh@(_, _, _, _, env)                        = case me of
                                                                Nothing -> (id, env) -- Return ignores acc, as it terminates the method
                                                                Just e  -> fExpStmt (Assign (NameLhs (Name [Ident "return"])) EqualA e) inh -- We treat "return e" as an assignment to the variable return
                                                            
    fSynchronized _                                     = fStmtBlock
    fThrow e inh@(acc, br, loop, catch, env)                 = case catch of
                                                                Nothing      -> ((\q -> q &* throwException e), env) -- acc is ignored, as the rest of the block is not executed
                                                                Just (cs, f) -> (maybe (if f then id else (\q -> q &* throwException e), env) (flip fStmtBlock (id, br, loop, Nothing, env)) (getCatch env e cs))
    fTry b cs f inh@(acc, br, loop, catch, env)              = let (r, env') = (fStmtBlock b (id, br, loop, Just (cs, isJust f), env)) in (r . maybe acc (fst . flip fStmtBlock (acc, br, loop, catch, env')) f, env) -- The finally-block is always executed
    fLabeled _ s                                        = s
    
    -- Helper functions
    
    -- A block also keeps track of the types of declared variables
    wlpBlock :: Inh -> BlockStmt -> Syn
    wlpBlock inh@(acc, br, loop, catch, env) b = case b of
                                                    BlockStmt s            -> wlp' inh s
                                                    LocalClass _           -> (acc, env)
                                                    LocalVars mods t vars  -> foldr (\v (r, env') -> (wlpDeclAssignment (r, br, loop, catch, env') v, env')) (acc, env) vars
                                                        
    -- Adds declarations within a block to a type environment
    envBlock :: [BlockStmt] -> TypeEnv -> TypeEnv
    envBlock bs env = foldl f env bs 
        where f env (LocalVars mods t vars) = foldr (\v env' -> (varName v, t):env') env vars
              f env _                       = env
              varName (VarDecl (VarId id) _) = Name [id]
                
    -- wlp of a var declaration that also assigns a value            
    wlpDeclAssignment :: Inh -> VarDecl -> Exp -> Exp
    wlpDeclAssignment (acc, _, _, _, _) (VarDecl _ Nothing) = acc
    wlpDeclAssignment (acc, _, _, _, _) (VarDecl (VarId ident) (Just (InitExp e))) = substVar (NameLhs (Name [ident])) e . acc
                        
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
    
    getCatch :: TypeEnv -> Exp -> [Catch] -> Maybe Block
    getCatch env e []             = Nothing
    getCatch env e (Catch p b:cs) = if catches env p e then Just b else getCatch env e cs
    
    -- Checks whether a catch block catches a certain error
    catches :: TypeEnv -> FormalParam -> Exp -> Bool
    catches env (FormalParam _ t _ _) e = t == RefType (ClassRefType (ClassType [(Ident "Exception", [])])) || 
                                          case e of
                                            ExpName name -> lookupType env name == t
                                            InstanceCreation _ t' _ _ -> t == RefType (ClassRefType t')
    
-- | The algebra that defines the wlp transformer for expressions with side effects
--   The first attribute is the expression itself (this is passed to handle substitutions in case of assignments)
wlpExpAlgebra :: ExpAlgebra (Inh -> (Exp, Syn))
wlpExpAlgebra = (fLit, fClassLit, fThis, fThisClass, fInstanceCreation, fQualInstanceCreation, fArrayCreate, fArrayCreateInit, fFieldAccess, fMethodInv, fArrayAccess, fExpName, fPostIncrement, fPostDecrement, fPreIncrement, fPreDecrement, fPrePlus, fPreMinus, fPreBitCompl, fPreNot, fCast, fBinOp, fInstanceOf, fCond, fAssign, fLambda, fMethodRef) where
    fLit lit (acc, _, _, _, env) = (Lit lit, (acc, env))
    fClassLit = undefined
    fThis = undefined
    fThisClass = undefined
    fInstanceCreation = undefined
    fQualInstanceCreation = undefined
    fArrayCreate t dimLengths dim inh@(acc, _, _, _, env) = (ArrayCreate t (map (flip getExp inh) dimLengths) dim, (acc, env))
    fArrayCreateInit t dim init inh@(acc, _, _, _, env) = (ArrayCreateInit t dim init, (acc, env))
    fFieldAccess = undefined
    fMethodInv = undefined
    fArrayAccess arrayIndex inh@(acc, _, _, _, env) = (ArrayAccess arrayIndex, (acc, env))
    fExpName name (acc, _, _, _, env) = (ExpName name, (acc, env))
    -- x++ increments x but evaluates to the original value
    fPostIncrement e inh@(acc, _, _, _, env) = case fst $ e inh of
                                                var@(ExpName name) -> (var, (substVar (NameLhs name) (BinOp var Add (Lit (Int 1))) . acc, env))
                                                exp  -> (exp, (acc, env))
    fPostDecrement e inh@(acc, _, _, _, env) = case fst $ e inh of
                                                var@(ExpName name) -> (var, (substVar (NameLhs name) (BinOp var Rem (Lit (Int 1))) . acc, env))
                                                exp  -> (exp, (acc, env))
    -- ++x increments x and evaluates to the new value of x
    fPreIncrement e inh@(acc, _, _, _, env) = case fst $ e inh of
                                                var@(ExpName name) -> (BinOp var Add (Lit (Int 1)), (substVar (NameLhs name) (BinOp var Add (Lit (Int 1))) . acc, env))
                                                exp  -> (BinOp exp Add (Lit (Int 1)), (acc, env))
    fPreDecrement e inh@(acc, _, _, _, env) = case fst $ e inh of
                                                var@(ExpName name) -> (BinOp var Rem (Lit (Int 1)), (substVar (NameLhs name) (BinOp var Rem (Lit (Int 1))) . acc, env))
                                                exp  -> (BinOp exp Rem (Lit (Int 1)), (acc, env))
    fPrePlus e inh@(acc, _, _, _, env)        = (fst $ e inh, (acc, env))
    fPreMinus e inh@(acc, _, _, _, env)       = (PreMinus . fst $ e inh, (acc, env))
    fPreBitCompl e inh@(acc, _, _, _, env)    = (PreBitCompl . fst $ e inh, (acc, env))
    fPreNot e inh@(acc, _, _, _, env)         = (PreNot . fst $ e inh, (acc, env))
    fCast = undefined
    fBinOp e1 op e2 inh@(acc, _, _, _, env)   = (BinOp (fst $ e1 inh) op (fst $ e2 inh), (acc, env)) 
    fInstanceOf = undefined
    fCond = undefined
    fAssign lhs op e inh@(acc, _, _, _, env)  = let rhs = desugarAssign lhs op (getExp e inh) in (rhs, (substVar lhs rhs . getTrans e inh, env))
    fLambda = undefined
    fMethodRef = undefined
    
    

-- | Gets the expression attribute
getExp :: (Inh -> (Exp, Syn)) -> Inh -> Exp
getExp f inh = let (e, _) = f inh in e

-- | Gets the transformer attribute
getTrans :: (Inh -> (Exp, Syn)) -> Inh -> Exp -> Exp
getTrans f inh = let (_, (trans, _)) = f inh in trans

-- | Gets the typeEnv attribute
getEnv :: (Inh -> (Exp, Syn)) -> Inh -> TypeEnv
getEnv f inh = let (_, (_, env)) = f inh in env

-- | Retrieves the type from the environment
lookupType :: TypeEnv -> Name -> Type
lookupType env name = fromJust (lookup name env)



    
-- | Calculates the weakest liberal pre-condition of a statement and a given post-condition
wlp :: Stmt -> Exp -> Exp
wlp = fst . (wlp' (id, id, id, Nothing, []))

-- wlp' lets you specify the inherited attributes
wlp' :: Inh -> Stmt -> Syn
wlp' inh s = foldStmt wlpStmtAlgebra s inh

