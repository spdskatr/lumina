module Lumina.Backend.CeliaToC (celiaToC) where

import Lumina.Middleend.Celia.Celia (CeliaTranslationUnit, CFunction (..), CVal (..), CType (..), CInstr (..), CBlockEnd (..), CFunctionDecl (..), CBlock (..))
import qualified Data.Map.Strict as Map
import Data.List (intercalate)
import Lumina.Middleend.Astra.Astra (UnaryOp(..))
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

getAllocTag :: CType -> String
getAllocTag CTBool = "0"
getAllocTag CTInt = "0"
getAllocTag CTPtr = "1"

emitCInstr :: CInstr -> String 
emitCInstr (CLoad cl cv) = show cl ++ " = " ++ emitCVal cv 
emitCInstr (CBinaryOp bo cl cv cv') = show cl ++ " = " ++ emitCVal cv  ++ " " ++ show bo ++ " " ++ emitCVal cv' ++ ";"
emitCInstr (CNot cl cv) = show cl ++ " = !" ++ emitCVal cv ++ ";"
emitCInstr (CDeRef cl cv ct) = show cl ++ " = DEREF(" ++ emitCVal cv ++ ", " ++ emitCType ct ++ ");"
emitCInstr (CMkRef cl cv ct) = show cl ++ " = MKREF(" ++ emitCVal cv ++ ", " ++ getAllocTag ct ++ ");"
emitCInstr (CCall cl f vs v) = show cl ++ " = " ++ showFuncName f ++ "(" ++ intercalate ", " (map emitCVal (v:vs)) ++ ");"
emitCInstr (CCallCl cl cl' v) = show cl ++ " = CALLCL(" ++ show cl' ++ ", " ++ emitCVal v ++ ");"
emitCInstr (CMkCl cl f vs) = show cl ++ " = MKCL(" ++ intercalate ", " (showFuncName f : map emitCVal vs) ++ ");"
emitCInstr (CSet cl cv) = "SET(" ++ show cl ++ ", " ++ emitCVal cv  ++ ");"
emitCInstr (CIncRef cl) = "INCREF(" ++ show cl ++ ");"
emitCInstr (CDecRef cl) = "DECREF(" ++ show cl ++ ");"

emitCBlockEnd :: CBlockEnd -> String
emitCBlockEnd (CGoto s) = "goto " ++ s ++ ";"
emitCBlockEnd (CBranch cl th el) = "BRANCH(" ++ show cl ++ ", " ++ th ++ ", " ++ el ++ ");"
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

celiaToC :: CeliaTranslationUnit -> String
celiaToC tu = "#include \"Lumina.h\"\n" ++ intercalate "\n" (Map.foldMapWithKey showDecl tu) ++ "\n\n" ++ intercalate "\n" (Map.foldMapWithKey showImpl tu)
    where 
        showDecl _ b = [emitCFunctionDecl (getDecl b) ++ ";"]
        showImpl _ b = [emitCFunction b]