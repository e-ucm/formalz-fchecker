module Types where

import Language.Java.Syntax
import Data.Maybe
import Data.List

import Folds
import HelperFunctions
import Settings


typesStmtAlgebra :: StmtAlgebra TypeEnv
typesStmtAlgebra = (fStmtBlock, fIfThen, fIfThenElse, fWhile, fBasicFor, fEnhancedFor, fEmpty, fExpStmt, fAssert, fSwitch, fDo, fBreak, fContinue, fReturn, fSynchronized, fThrow, fTry, fLabeled) where
    fStmtBlock b                    = envBlock b
    fIfThen e s1                    = s1
    fIfThenElse e s1 s2             = s1 ++ s2
    fWhile e s                      = s
    fBasicFor init me incr s        = case init of
                                        Just (ForLocalVars mods t vars) -> foldr (\v env' -> (varName v, t):env') s vars
                                        _ -> s
    fEnhancedFor                    = error "EnhancedFor"
    fEmpty                          = []
    fExpStmt e                      = []    
    fAssert e _                     = []
    fSwitch e bs                    = concatMap envSwitch bs
    fDo s e                         = s
    fBreak _                        = []
    fContinue _                     = []
    fReturn me                      = []
    fSynchronized _ _               = []
    fThrow e                        = []
    fTry b cs f                     = envBlock b ++ concatMap envCatch cs ++ 
                                        (case f of
                                            Just b  -> envBlock b
                                            _       -> [])
    fLabeled _ s                    = s
    
                                                        
    -- Adds declarations within a block to a type environment
    envBlock :: Block -> TypeEnv
    envBlock (Block bs) = foldl f [] bs 
        where f env (LocalVars mods t vars)  = foldr (\v env' -> (varName v, t):env') env vars
              f env (BlockStmt s)            = foldStmt typesStmtAlgebra s ++ env
              f env _                        = env
              
    varName (VarDecl (VarId id) _) = Name [id]
                                                        
    -- Adds declarations within a switchblock block to a type environment
    envSwitch :: SwitchBlock -> TypeEnv
    envSwitch (SwitchBlock _ bs) = envBlock (Block bs)
    
    -- Adds declarations within a catchblock block to a type environment
    envCatch :: Catch -> TypeEnv
    envCatch (Catch (FormalParam _ t _ varId) b) = (Name [getId varId], t) : envBlock b

    
-- Gets the types of all variables that are declared in a statement
getStmtTypeEnv :: Stmt -> TypeEnv
getStmtTypeEnv = foldStmt typesStmtAlgebra

-- Gets the parameters of a function and puts them in the type environment
getMethodTypeEnv :: [TypeDecl] -> Ident -> TypeEnv
getMethodTypeEnv decls id = case getMethodParams decls id of
                                Nothing     -> []
                                Just params -> map fromParam params
  where
    fromParam (FormalParam _ t _ paramId) = (Name [getId paramId], t)

-- Gets the static member declarations and puts them in the type environment
getStaticVarsTypeEnv :: CompilationUnit -> TypeEnv
getStaticVarsTypeEnv compUnit = concatMap fromTypeDecls (getDecls compUnit) where
    fromTypeDecls (ClassTypeDecl (ClassDecl _ _ _ _ _ (ClassBody decls))) = concatMap fromMemberDecl decls
    fromMemberDecl (MemberDecl (FieldDecl mods t varDecls)) = if Static `elem` mods then map (fromVarDecl t) varDecls else []
    fromMemberDecl _                                        = []
    fromVarDecl t (VarDecl varId _) = (Name [getId varId], t)
    
-- Gets the type env for a compilation unit
getTypeEnv :: CompilationUnit -> [TypeDecl] -> [Ident] -> TypeEnv
getTypeEnv compUnit decls methods = getStaticVarsTypeEnv compUnit ++ concatMap (getMethodTypeEnv decls) methods ++ concatMap (getStmtTypeEnv . fromJust . getMethod decls) methods