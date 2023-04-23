module Lumina.Backend.CeliaToC (celiaToC, getFlags) where

import Lumina.Middleend.Celia.Celia (CeliaTranslationUnit, CFunction (..), CVal (..), CType (..), CInstr (..), CBlockEnd (..), CFunctionDecl (..), CBlock (..), CLoc)
import qualified Data.Map.Strict as Map
import Data.List (intercalate)
import Lumina.Utils (indent)

{-
 - Turn the Celia IR into valid C code. Additionally:
 - 
 - * Include the runtime header
 - * Forward-declare functions
 - * Specify which function is an entrypoint
 -}

emitCVal :: CVal -> String
emitCVal (CVar l) = show l
emitCVal (CBool False) = "0"
emitCVal (CBool True) = "1"
emitCVal (CInt i) = show i

emitCType :: CType -> String
emitCType CTBool = "int64_t"
emitCType CTInt = "int64_t"
emitCType CTPtr = "Ref*"

showFuncName :: String -> String
showFuncName s = "f_" ++ s

getAllocTag :: CType -> Char
getAllocTag CTBool = '0'
getAllocTag CTInt = '0'
getAllocTag CTPtr = '1'

getFlags :: [(CVal, CType)] -> String
getFlags vs = reverse $ '0' : [getAllocTag t | (_,t) <- vs]

emitMkCl :: CLoc -> String -> [(CVal, CType)] -> String
emitMkCl cl f vs = "MKCL(" ++ show cl ++ ", " ++ show (length flags) ++ ", 0b" ++ flags ++ ", " ++ intercalate ", " elems ++ ");"
    where
        flags = getFlags vs
        elems = ("(uint64_t)cl_" ++ showFuncName f) : [ "(uint64_t)" ++ emitCVal v | (v,_) <- vs]

emitCInstr :: CInstr -> String 
emitCInstr (CLoad cl cv) = show cl ++ " = " ++ emitCVal cv ++ ";"
emitCInstr (CBinaryOp bo cl cv cv') = show cl ++ " = " ++ emitCVal cv  ++ " " ++ show bo ++ " " ++ emitCVal cv' ++ ";"
emitCInstr (CNot cl cv) = show cl ++ " = !" ++ emitCVal cv ++ ";"
emitCInstr (CDeRef cl cv ct) = show cl ++ " = DEREF(" ++ emitCVal cv ++ ", " ++ emitCType ct ++ ");"
emitCInstr (CMkRef cl cv ct) = show cl ++ " = MKREF(" ++ emitCVal cv ++ ", " ++ [getAllocTag ct] ++ ");"
emitCInstr (CCall cl f vs v) = show cl ++ " = " ++ showFuncName f ++ "(" ++ intercalate ", " (map emitCVal (v:vs)) ++ ");"
emitCInstr (CCallCl t cl cl' v) = show cl ++ " = CALLCL(" ++ emitCType t ++ ", " ++ show cl' ++ ", (uint64_t)" ++ emitCVal v ++ ");"
emitCInstr (CMkCl cl f vs) = emitMkCl cl f vs
emitCInstr (CSet cl cv) = "SET(" ++ show cl ++ ", " ++ emitCVal cv  ++ ");"
emitCInstr (CIncRef cl) = "INCREF(" ++ show cl ++ ");"
emitCInstr (CDecRef cl) = "DECREF(" ++ show cl ++ ");"

emitCBlockEnd :: CBlockEnd -> String
emitCBlockEnd (CGoto s) = "goto " ++ s ++ ";"
emitCBlockEnd (CBranch cl th el) = "BRANCH(" ++ emitCVal cl ++ ", " ++ th ++ ", " ++ el ++ ");"
emitCBlockEnd (CReturn v) = "return " ++ emitCVal v ++ ";"

emitCBlock :: CBlock -> String
emitCBlock (CBlock is en) = intercalate "\n" (map emitCInstr is ++ [emitCBlockEnd en])

emitCFunctionDecl :: CFunctionDecl -> String
emitCFunctionDecl (CFunctionDecl f t' args) = emitCType t' ++ " " ++ showFuncName f ++ " (" ++ intercalate ", " (map showArg args) ++ ")"
    where showArg (x, t) = emitCType t ++ " " ++ show x

emitCFunction :: CFunction -> String
emitCFunction (CFunction decl locs bls) = emitCFunctionDecl decl ++ " {\n" ++ body ++ "}\n"
    where
        body = indent (intercalate "\n" (Map.foldMapWithKey showLoc locs)) ++ "\n" ++ intercalate "\n" (Map.foldMapWithKey showBlock bls)
        showLoc loc t = [emitCType t ++ " " ++ show loc ++ ";"]
        showBlock name bl = [name ++ ":\n" ++ indent (emitCBlock bl)]

emitClosuredCFunctionDecl :: CFunctionDecl -> String
emitClosuredCFunctionDecl (CFunctionDecl f t' _) = emitCType t' ++ " cl_" ++ showFuncName f ++ "(Ref* env, uint64_t arg)";

emitClosuredCFunction :: CFunctionDecl -> String
emitClosuredCFunction (CFunctionDecl f t' args) = emitClosuredCFunctionDecl (CFunctionDecl f t' args) ++ " {\n" ++ indent body ++ "}\n"
    where
        body = "return " ++ showFuncName f ++ "(" ++ intercalate ", " unpackedArgs ++ ");"
        unpackedArgs = case args of
          (_,t) : rest -> ("(" ++ emitCType t ++ ")arg") : [ "(" ++ emitCType t2 ++ ")env[" ++ show (i :: Int) ++ "]" | ((_,t2),i) <- zip rest [0..] ]
          [] -> []

celiaToC :: CeliaTranslationUnit -> String
celiaToC tu = "#include \"Lumina.h\"\n" ++ intercalate "\n" (Map.foldMapWithKey showDecl tu) ++ "\n\n" ++ intercalate "\n" (Map.foldMapWithKey showImpl tu)
    where 
        showDecl _ b = [emitCFunctionDecl (getDecl b) ++ ";", emitClosuredCFunctionDecl (getDecl b) ++ ";"]
        showImpl _ b = [emitCFunction b, emitClosuredCFunction (getDecl b)]