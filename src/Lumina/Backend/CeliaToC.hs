module Lumina.Backend.CeliaToC (celiaToC) where

import Lumina.Middleend.Celia.Celia (CeliaTranslationUnit, CFunction (..))
import qualified Data.Map.Strict as Map
import Data.List (intercalate)

{-
 - By Default, the `Show` typeclass implementation for Celia code implements
 - emitting of C code. However, a few extra steps are required to make the code
 - a valid C translation unit:
 - 
 - * Include the runtime header
 - * Forward-declare functions
 - * Specify which function is an entrypoint
 -}

celiaToC :: CeliaTranslationUnit -> String
celiaToC tu = "#include \"Lumina.h\"\n" ++ intercalate "\n" (Map.foldMapWithKey showDecl tu) ++ "\n\n" ++ intercalate "\n" (Map.foldMapWithKey showImpl tu)
    where 
        showDecl _ b = [show (getDecl b) ++ ";"]
        showImpl _ b = [show b]