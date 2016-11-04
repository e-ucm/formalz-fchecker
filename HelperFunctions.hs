-- Helper functions for the Java data structure
module HelperFunctions where

import Language.Java.Syntax
import System.IO
import System.IO.Unsafe
import Data.IORef


heap :: Exp
heap = FieldAccess (ClassFieldAccess (Name []) (Ident "<heap>"))

heapPointer :: IORef Integer
heapPointer = unsafePerformIO $ newIORef 0

-- | Gets the current heap pointer and increases the pointer by 1
getIncrPointer :: IORef Integer -> Integer
getIncrPointer ref = unsafePerformIO $
    do
        p <- readIORef ref
        writeIORef ref (p + 1)
        return p

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

(==*) :: Exp -> Exp -> Exp
e1 ==* e2 = BinOp e1 Equal e2

(/=*) :: Exp -> Exp -> Exp
e1 /=* e2 = neg (e1 ==* e2)

-- | Gets the initial value for a given type
getInitValue :: Type -> Exp
getInitValue (PrimType t) = case t of
                                BooleanT -> Lit (Boolean False)
                                ByteT -> Lit (Word 0)
                                ShortT -> Lit (Int 0)
                                IntT -> Lit (Int 0)
                                LongT -> Lit (Int 0)
                                CharT -> Lit (Char '\NUL')
                                FloatT -> Lit (Float 0)
                                DoubleT -> Lit (Double 0)
getInitValue (RefType t)  = Lit Null