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
    

-- | A type for the inherited attribute
data Inh = Inh {acc     :: Exp -> Exp,  -- The accumulated transformer of the current block up until the current statement
                br      :: Exp -> Exp,  -- The accumulated transformer up until the last loop (this is used when handling break statements etc.)
                loop    :: Exp -> Exp,  -- The wlp of the current loop statement not including initialization code. It refers to the loop starting from the loop continuation point.
                catch   :: Maybe ([Catch], Bool), -- The catches when executing a block in a try statement, and a Bool indicating wether there is a finally-block
                env     :: TypeEnv,     -- The type environment for typing expressions
                decls   :: [ClassDecl]} -- Class declarations
            
           
-- | A type synonym for the synthesized attributea
type Syn = (Exp -> Exp, -- The wlp transformer
            TypeEnv)    -- The type environment

type TypeEnv = [(Name, Type)]
    
-- | The algebra that defines the wlp transformer for statements
--   The synthesized attribute is the resulting transformer. 
--   Statements that pass control to the next statement have to explicitly combine their wlp function with the accumulated function, as some statements (e.g. break) ignore the accumulated function.
wlpStmtAlgebra :: StmtAlgebra (Inh -> Syn)
wlpStmtAlgebra = (fStmtBlock, fIfThen, fIfThenElse, fWhile, fBasicFor, fEnhancedFor, fEmpty, fExpStmt, fAssert, fSwitch, fDo, fBreak, fContinue, fReturn, fSynchronized, fThrow, fTry, fLabeled) where
    fStmtBlock (Block bs) inh       = foldr (\b (r, env') -> wlpBlock (inh {acc = r, env = env'}) b) (acc inh, envBlock bs (env inh)) bs -- The result of the last block-statement will be the accumulated transformer for the second-last etc. The type environment is build from the left, so it has to be done seperately.
    fIfThen e s1                    = fIfThenElse e s1 (const (id, [])) -- if-then is just an if-then-else with an empty else-block
    fIfThenElse e s1 s2 inh         = ((\q -> (e &* fst (s1 inh) q) |* (neg e &* fst (s2 inh) q)) . acc inh, env inh)
    fWhile e s inh                  = let loop' = (\q -> if unsafeIsTrue (((inv &* neg e) `imp` q) &* ((inv &* e) `imp` fst (s (inh {loop = loop'})) inv)) then inv else (neg e &* q)) in (loop' . acc inh, env inh)
    fBasicFor init me incr s inh    = let loop' = fst (fWhile (fromMaybeGuard me) (\inh' -> s (inh {acc = fst (wlp' inh' (incrToStmt incr)), loop = loop'})) inh) in wlp' (inh {acc = loop'}) (initToStmt init)
    fEnhancedFor                    = error "TODO: EnhancedFor"
    fEmpty inh                      = (acc inh, env inh) -- Empty does nothing, but still passes control to the next statement
    fExpStmt e inh                  = snd $ foldExp wlpExpAlgebra e inh
    fAssert e _ inh                 = ((e &*) . acc inh, env inh)
    fSwitch e bs inh                = let (e', s1, s2) = desugarSwitch e bs in fIfThenElse e' (flip wlp' s1) (flip wlp' s2) (inh {acc = id, br = acc inh})
    fDo s e inh                     = let loop' = fst (s (inh {acc = fst (fWhile e s (inh {loop = loop'})), loop = loop'})) in (loop', env inh) -- Do is just a while with the statement block executed one additional time. Break and continue still have to be handled in this additional execution.
    fBreak _ inh                    = (br inh, env inh) -- wlp of the breakpoint
    fContinue _ inh                 = (loop inh, env inh) -- wlp of the loop
    fReturn me inh                  = case me of
                                        Nothing -> (id, env inh) -- Return ignores acc, as it terminates the method
                                        Just e  -> fExpStmt (Assign (NameLhs (Name [Ident "return"])) EqualA e) inh -- We treat "return e" as an assignment to the variable return
                                                            
    fSynchronized _                 = fStmtBlock
    fThrow e inh                    = case catch inh of
                                        Nothing      -> ((\q -> q &* throwException e), env inh) -- acc is ignored, as the rest of the block is not executed
                                        Just (cs, f) -> (maybe (if f then id else (\q -> q &* throwException e), env inh) (flip fStmtBlock (inh {acc = id, catch = Nothing})) (getCatch (env inh) e cs))
    fTry b cs f inh                 = let (r, env') = (fStmtBlock b (inh {acc = id, catch = Just (cs, isJust f)})) in (r . maybe (acc inh) (fst . flip fStmtBlock (inh {env = env'})) f, env inh) -- The finally-block is always executed
    fLabeled _ s                    = s
    
    -- Helper functions
    
    -- A block also keeps track of the types of declared variables
    wlpBlock :: Inh -> BlockStmt -> Syn
    wlpBlock inh b  = case b of
                        BlockStmt s            -> wlp' inh s
                        LocalClass _           -> (acc inh, env inh)
                        LocalVars mods t vars  -> foldr (\v (r, env') -> (wlpDeclAssignment t (inh {acc = r, env = env'}) v, env')) (acc inh, env inh) vars
                                                        
    -- Adds declarations within a block to a type environment
    envBlock :: [BlockStmt] -> TypeEnv -> TypeEnv
    envBlock bs env = foldl f env bs 
        where f env (LocalVars mods t vars) = foldr (\v env' -> (varName v, t):env') env vars
              f env _                       = env
              varName (VarDecl (VarId id) _) = Name [id]
                
    -- wlp of a var declaration that also assigns a value. Declaring without assignment assigns the default value
    wlpDeclAssignment :: Type -> Inh -> VarDecl -> Exp -> Exp
    wlpDeclAssignment t inh (VarDecl (VarId ident) Nothing)             = substVar (NameLhs (Name [ident])) (getInitValue t) . acc inh
    wlpDeclAssignment t inh (VarDecl (VarId ident) (Just (InitExp e)))  = substVar (NameLhs (Name [ident])) e . acc inh
                        
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
                                                    Default -> (true, StmtBlock (Block (bs ++ [BlockStmt sbscode])), sbscode)
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
    fLit lit inh = (Lit lit, (acc inh, env inh))
    fClassLit = undefined
    fThis = undefined
    fThisClass = undefined
    fInstanceCreation typeArgs t args mBody inh         = let p = getIncrPointer heapPointer in (ArrayAccess (ArrayIndex heap [Lit (Int p)]), (substVar (ArrayLhs (ArrayIndex heap [Lit (Int p)])) (makeObjectArray (decls inh) t args mBody) . acc inh, env inh)) -- TODO: assign default values to fields. basically the WLP of: heap[p] = new object[#fields]
    fQualInstanceCreation e typeArgs t args mBody inh   = (QualInstanceCreation (getExp e inh) typeArgs t args mBody, (getTrans e inh, env inh))
    fArrayCreate t dimLengths dim inh                   = (ArrayCreate t (map (flip getExp inh) dimLengths) dim, (acc inh, env inh))
    fArrayCreateInit t dim init inh                     = (ArrayCreateInit t dim init, (acc inh, env inh))
    fFieldAccess fieldAccess inh                        = case fieldAccess of
                                                            PrimaryFieldAccess e (Ident field) -> (ArrayAccess (ArrayIndex e [Lit (String field)]), (acc inh, env inh)) -- Objects are modelled as arrays
                                                            _ -> undefined
    fMethodInv                                          = error "method call"
    fArrayAccess arrayIndex inh                         = (ArrayAccess arrayIndex, (acc inh, env inh))
    fExpName name inh                                   = (ExpName name, (acc inh, env inh))
    -- x++ increments x but evaluates to the original value
    fPostIncrement e inh                                = case getExp e inh of
                                                            var@(ExpName name) -> (var, (substVar (NameLhs name) (BinOp var Add (Lit (Int 1))) . acc inh, env inh))
                                                            exp  -> (exp, (acc inh, env inh))
    fPostDecrement e inh                                = case getExp e inh of
                                                            var@(ExpName name) -> (var, (substVar (NameLhs name) (BinOp var Rem (Lit (Int 1))) . acc inh, env inh))
                                                            exp  -> (exp, (acc inh, env inh))
    -- ++x increments x and evaluates to the new value of x
    fPreIncrement e inh                                 = case getExp e inh of
                                                            var@(ExpName name) -> (BinOp var Add (Lit (Int 1)), (substVar (NameLhs name) (BinOp var Add (Lit (Int 1))) . acc inh, env inh))
                                                            exp  -> (BinOp exp Add (Lit (Int 1)), (acc inh, env inh))
    fPreDecrement e inh                                 = case getExp e inh of
                                                            var@(ExpName name) -> (BinOp var Rem (Lit (Int 1)), (substVar (NameLhs name) (BinOp var Rem (Lit (Int 1))) . acc inh, env inh))
                                                            exp  -> (BinOp exp Rem (Lit (Int 1)), (acc inh, env inh))
    fPrePlus e inh                                      = (getExp e inh, (acc inh, env inh))
    fPreMinus e inh                                     = (PreMinus $ getExp e inh, (acc inh, env inh))
    fPreBitCompl e inh                                  = (PreBitCompl $ getExp e inh, (acc inh, env inh))
    fPreNot e inh                                       = (PreNot $ getExp e inh, (acc inh, env inh))
    fCast t e inh                                       = (getExp e inh, (acc inh, env inh))
    fBinOp e1 op e2 inh                                 = (BinOp (getExp e1 inh) op (getExp e2 inh), (getTrans e1 (inh {acc = getTrans e2 inh}), env inh)) 
    fInstanceOf                                         = error "TODO: instanceOf"
    fCond g e1 e2 inh                                   = (Cond (getExp g inh) (getExp e1 inh) (getExp e2 inh), (getTrans g (inh {acc = id}) . (\q -> (getExp g inh &* getTrans e1 (inh {acc = id}) q) |* (neg (getExp g inh) &* getTrans e2 (inh {acc = id}) q)) . acc inh, env inh))
    fAssign lhs op e inh                                = let rhs = desugarAssign lhs op (getExp e inh) in (rhs, (substVar lhs rhs . getTrans e inh, env inh))
    fLambda                                             = error "lambda"
    fMethodRef                                          = error "method reference"
    
    

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


-- | Creates an array that represents an object
makeObjectArray :: [ClassDecl] -> ClassType -> [Argument] -> (Maybe ClassBody) -> Exp
makeObjectArray decls t args mBody = undefined 

-- Initializes the heap
initHeap :: Exp
initHeap = Assign (NameLhs (Name [Ident "<heap>"])) EqualA (ArrayCreate (RefType undefined) [Lit (Int 10000)] 0)
    
-- | Calculates the weakest liberal pre-condition of a statement and a given post-condition
wlp :: Stmt -> Exp -> Exp
wlp = fst . (wlp' (Inh id id id Nothing [] undefined))

-- wlp' lets you specify the inherited attributes
wlp' :: Inh -> Stmt -> Syn
wlp' inh s = foldStmt wlpStmtAlgebra s inh

