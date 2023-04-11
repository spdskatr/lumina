module Lumina.Middleend.Celia.Celia where

import Lumina.Middleend.Astra.Astra (BinaryOp, UnaryOp)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Lumina.Middleend.Mona.Mona (MonaFunction (..), MExpr (..), MAtom (..), MOper (..))
import Lumina.Utils (internalError)
import Lumina.Middleend.Typing (LuminaType (..))
import Control.Monad (forM_)

{-
 - Celia is an Cmm-like IR. That is, the code should be trivially translatable
 - into C code. This means manual memory management and garbage collection.
 - Also, unit types shouldn't exist beyond this point.
 -
 - It should also translate fairly cleanly to LLVM (I hope)
 -
 - It's the third step in the pipeline (Astra -> Mona -> Celia).
 -}

newtype CLoc = CLoc String deriving (Eq, Show, Ord)

celiaError :: String -> a
celiaError s = internalError ("Could not convert to Celia IR: " ++ s)

data CVal
    = CVar CLoc
    | CBool Bool
    | CInt Int

data CType = CTBool | CTInt | CTPtr

data CInstr
    = CLoad CLoc CVal
    | CBinaryOp BinaryOp CLoc CVal CVal
    | CUnaryOp UnaryOp CLoc CVal
    | CCall CLoc String [CVal]
    | CCallCl CLoc CLoc CVal
    | CMkCl CLoc String [CVal]
    | CSet CLoc CVal
    | CIncRef CLoc
    | CDecRef CLoc

data CBlockEnd
    = CGoto String
    | CBranch CLoc String String
    | CReturn CVal

data CBlock = CBlock [CInstr] CBlockEnd

-- First element of args is the actual argument, following args are the env
data CFunctionDecl = CFunctionDecl { getName :: String, getArgs :: [(CLoc, CType)] }

data CFunction = CFunction { getDecl :: CFunctionDecl, getLocals :: Map CLoc CType, getBlocks :: Map String CBlock }

data CeliaBuilder a = CeliaBuilder 
    { getBuilderLocals :: Map CLoc CType
    , getBuilderBlocks :: Map String CBlock
    , getInstrs :: [CInstr]
    , getInner :: a }

instance Functor CeliaBuilder where
    fmap f cb = cb { getInner = f (getInner cb) }

instance Applicative CeliaBuilder where
    pure = CeliaBuilder Map.empty Map.empty []
    (CeliaBuilder ls1 bs1 is1 f) <*> (CeliaBuilder ls2 bs2 is2 x) = CeliaBuilder (Map.union ls1 ls2) (Map.union bs1 bs2) (is1 ++ is2) (f x)

instance Monad CeliaBuilder where
    (CeliaBuilder ls1 bs1 is1 x) >>= f =
        let (CeliaBuilder ls2 bs2 is2 y) = f x
        in CeliaBuilder (Map.union ls1 ls2) (Map.union bs1 bs2) (is1 ++ is2) y

newBlock :: String -> CBlock -> CeliaBuilder ()
newBlock s b = CeliaBuilder Map.empty (Map.singleton s b) [] ()

newLocal :: CLoc -> CType -> CeliaBuilder ()
newLocal x t = CeliaBuilder (Map.singleton x t) Map.empty [] ()

emitCInstr :: CInstr -> CeliaBuilder ()
emitCInstr i = CeliaBuilder Map.empty Map.empty [i] ()

buildBlock :: CeliaBuilder CBlockEnd -> CeliaBuilder CBlock
buildBlock (CeliaBuilder ls bs is e) = CeliaBuilder ls bs [] (CBlock is e)

getCType :: LuminaType -> CType
getCType TInt = CTInt
getCType TBool = CTBool
getCType TUnit = CTInt
getCType (TFun _ _) = CTPtr
getCType (TRef _) = CTPtr

getCVal :: MAtom -> CVal
getCVal (MVar s) = CVar (CLoc s)
getCVal MUnit = CInt 0
getCVal (MBool b) = CBool b
getCVal (MInt i) = CInt i

exprToCelia :: Map String CFunctionDecl -> [CLoc] -> String -> MExpr -> CeliaBuilder CBlockEnd
exprToCelia ds rs name (MLet x t o rest) = do
    newLocal (CLoc x) (getCType t)
    let instr = case o of
            MUnary uo ma -> CUnaryOp uo (CLoc x) (getCVal ma)
            MBinary bo ma ma' -> CBinaryOp bo (CLoc x) (getCVal ma) (getCVal ma')
            MApp ma ma' -> case ma of
                MVar c -> case ds Map.!? c of
                    Nothing -> CCallCl (CLoc x) (CLoc c) (getCVal ma')
                    Just decl -> CCall (CLoc x) c (getCVal ma' : [CVar a | (a,_) <- tail (getArgs decl)])
                _ -> celiaError ("Tried to call a function that is definitely not a function; found " ++ show ma)
            MCall f vs ma -> undefined
            MMkClosure s as -> undefined
            MJust _ -> celiaError ("Tried to translate MJust expression " ++ show o ++ " which should have been removed during optimisation.")
    emitCInstr instr
    exprToCelia ds rs name rest
exprToCelia ds rs name (MAssign a b rest) = do
    case a of
        MVar a' -> do
            emitCInstr $ CSet (CLoc a') (getCVal b)
        _ -> celiaError ("Found an assignment expression to an item that isn't a variable; found " ++ show a)
    exprToCelia ds rs name rest
exprToCelia ds rs name (MIf a th el) = do
    let thenName = name ++ "_then"
    let elseName = name ++ "_else"
    thenBlock <- buildBlock $ exprToCelia ds rs thenName th
    elseBlock <- buildBlock $ exprToCelia ds rs elseName el
    newBlock thenName thenBlock
    newBlock elseName elseBlock
    case a of
        MVar x -> 
            return $ CBranch (CLoc x) thenName elseName
        _ -> celiaError ("Found if-statement on a non-variable " ++ show a ++ ". Did we forget to optimise the code before translating it?")
exprToCelia _ rs _ (MReturn a) = do
    let passOn = case a of
            MVar x -> [y | y <- rs, CLoc x /= y]
            _ -> rs
    forM_ passOn (emitCInstr . CDecRef)
    return $ CReturn (getCVal a)