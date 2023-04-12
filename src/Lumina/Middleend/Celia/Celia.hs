module Lumina.Middleend.Celia.Celia (
    CVal (..),
    CInstr (..),
    CType (..),
    CBlockEnd (..),
    CBlock (..),
    CFunctionDecl (..),
    CFunction (..),
    CeliaTranslationUnit,
    showTranslationUnit,
    monaToCelia
) where

import Lumina.Middleend.Astra.Astra (BinaryOp, UnaryOp (..))

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Lumina.Middleend.Mona.Mona (MonaFunction (..), MExpr (..), MAtom (..), MOper (..), MonaTranslationUnit)
import Lumina.Utils (internalError, indent)
import Lumina.Middleend.Typing (LuminaType (..))
import Control.Monad (forM_)
import Lumina.Middleend.Astra.HoistFunctions (TypedVar(..))
import Data.List (intercalate)

{-
 - Celia is an Cmm-like IR. That is, the code should be trivially translatable
 - into C code. This means manual memory management and garbage collection.
 - Also, unit types shouldn't exist beyond this point.
 -
 - It should also translate fairly cleanly to LLVM (I hope)
 -
 - It's the third step in the pipeline (Astra -> Mona -> Celia).
 -}

newtype CLoc = CLoc String deriving (Eq, Ord)
instance Show CLoc where
    show (CLoc s) = "_" ++ s

celiaError :: String -> a
celiaError s = internalError ("Could not convert to Celia IR: " ++ s)

data CVal
    = CVar CLoc
    | CBool Bool
    | CInt Int

instance Show CVal where
    show (CVar l) = show l
    show (CBool False) = "false"
    show (CBool True) = "true"
    show (CInt i) = show i

data CType = CTBool | CTInt | CTPtr

instance Show CType where
    show CTBool = "bool"
    show CTInt = "int"
    show CTPtr = "Ref*"

data CInstr
    = CLoad CLoc CVal
    | CBinaryOp BinaryOp CLoc CVal CVal
    | CUnaryOp UnaryOp CLoc CVal
    | CCall CLoc String [CVal] CVal
    | CCallCl CLoc CLoc CVal
    | CMkCl CLoc String [CVal]
    | CSet CLoc CVal
    | CIncRef CLoc
    | CDecRef CLoc

showUnaryOpInC :: UnaryOp -> CVal -> String
showUnaryOpInC OpNot cv = "!" ++ show cv
showUnaryOpInC OpRef cv = "MKREF(" ++ show cv ++ ")"
showUnaryOpInC OpBang cv = "DEREF(" ++ show cv ++ ")"

showFuncName :: String -> String
showFuncName s = "f_" ++ s

instance Show CInstr where
    show (CLoad cl cv) = show cl ++ " = " ++ show cv
    show (CBinaryOp bo cl cv cv') = show cl ++ " = " ++ show cv ++ " " ++ show bo ++ " " ++ show cv' ++ ";"
    show (CUnaryOp uo cl cv) = show cl ++ " = " ++ showUnaryOpInC uo cv ++ ";"
    show (CCall cl f vs v) = show cl ++ " = " ++ showFuncName f ++ "(" ++ intercalate ", " (map show (v:vs)) ++ ");"
    show (CCallCl cl cl' v) = show cl ++ " = CALLCL(" ++ show cl' ++ ", " ++ show v ++ ");"
    show (CMkCl cl f vs) = show cl ++ " = MKCL(" ++ intercalate ", " (showFuncName f : map show vs) ++ ");"
    show (CSet cl cv) = "SET(" ++ show cl ++ ", " ++ show cv ++ ");"
    show (CIncRef cl) = "INCREF(" ++ show cl ++ ");"
    show (CDecRef cl) = "DECREF(" ++ show cl ++ ");"

data CBlockEnd
    = CGoto String
    | CBranch CLoc String String
    | CReturn CVal

instance Show CBlockEnd where
    show (CGoto s) = "goto " ++ s ++ ";"
    show (CBranch cl th el) = "BRANCH(" ++ show cl ++ ", " ++ th ++ ", " ++ el ++ ");"
    show (CReturn v) = "return " ++ show v ++ ";"

data CBlock = CBlock [CInstr] CBlockEnd
instance Show CBlock where
    show (CBlock is en) = intercalate "\n" (map show is ++ [show en])

-- First element of args is the actual argument, following args are the env
data CFunctionDecl = CFunctionDecl { getCName :: String, getReturnType :: CType, getCArgs :: [(CLoc, CType)] }

instance Show CFunctionDecl where
    show (CFunctionDecl f t' args) = show t' ++ " " ++ showFuncName f ++ " (" ++ intercalate ", " (map showArg args) ++ ")"
        where showArg (x, t) = show t ++ " " ++ show x

data CFunction = CFunction 
    { getDecl :: CFunctionDecl
    , getLocals :: Map CLoc CType
    , getBlocks :: Map String CBlock }

instance Show CFunction where
    show (CFunction decl locs bls) = show decl ++ " {\n" ++ body ++ "}\n"
        where
            body = indent (intercalate "\n" (Map.foldMapWithKey showLoc locs)) ++ "\n" ++ intercalate "\n" (Map.foldMapWithKey showBlock bls)
            showLoc loc t = [show t ++ " " ++ show loc ++ ";"]
            showBlock name bl = [name ++ ":\n" ++ indent (show bl)]

type CeliaTranslationUnit = Map String CFunction

showTranslationUnit :: CeliaTranslationUnit -> String
showTranslationUnit tu = "#include \"Lumina.h\"\n" ++ intercalate "\n" (Map.foldMapWithKey showDecl tu) ++ "\n" ++ intercalate "\n" (Map.foldMapWithKey showImpl tu)
    where 
        showDecl _ b = [show (getDecl b)]
        showImpl _ b = [show b]

data CeliaBuilder a = CeliaBuilder (Map CLoc CType) (Map String CBlock) [CInstr] a

instance Functor CeliaBuilder where
    fmap f (CeliaBuilder ls bs is a) = CeliaBuilder ls bs is (f a)

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
                MVar c -> CCallCl (CLoc x) (CLoc c) (getCVal ma')
                _ -> celiaError ("Tried to call a function that is definitely not a function; found " ++ show ma)
            MCall f vs ma -> CCall (CLoc x) f (getCVal <$> vs) (getCVal ma)
            MMkClosure s as -> CMkCl (CLoc x) s (getCVal <$> as)
            MJust a -> CLoad (CLoc x) (getCVal a)
    emitCInstr instr
    exprToCelia ds rs name rest
exprToCelia ds rs name (MAssign a b rest) = do
    case a of
        MVar a' -> do
            emitCInstr $ CSet (CLoc a') (getCVal b)
        _ -> celiaError ("Found an assignment expression to an item that isn't a variable; found " ++ show a)
    exprToCelia ds rs name rest
exprToCelia ds rs name (MIf a th el) = do
    let thenName = name ++ "_a"
    let elseName = name ++ "_b"
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

toCDecl :: MonaFunction -> CFunctionDecl
toCDecl mf = CFunctionDecl (getName mf) (getCType $ getResultType mf) ((CLoc (getArg mf), getCType (getArgType mf)) : [(CLoc x, getCType t) | (TypedVar x t) <- getFV mf])

toCFunction :: Map String CFunctionDecl -> MonaFunction -> CFunction
toCFunction ds mf = 
    let (MonaFunction name _ _ _ _ b) = mf
        decl = ds Map.! name
        entryname = "l_" ++ name ++ "_entry"
        (CeliaBuilder ls bs _ bl) = buildBlock $ exprToCelia ds [] entryname b
        allBlocks = Map.insert entryname bl bs
    in CFunction decl ls allBlocks

monaToCelia :: MonaTranslationUnit -> CeliaTranslationUnit
monaToCelia fs = 
    let decls = Map.map toCDecl fs
    in Map.map (toCFunction decls) fs