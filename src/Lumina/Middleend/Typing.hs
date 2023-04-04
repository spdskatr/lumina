module Lumina.Middleend.Typing (LuminaType(..), (~), getReturnType) where

import Lumina.Utils (internalError)

-- I have to add a TAny type to make sure type checking for CPS works out.
-- Maybe I'll implement HM type inference some time soon.
data LuminaType
    = TAny
    | TInt
    | TBool
    | TUnit
    | TRef LuminaType
    | TFun LuminaType LuminaType
    deriving Eq

instance Show LuminaType where
    show TAny = "*"
    show TInt = "int"
    show TBool = "bool"
    show TUnit = "unit"
    show (TRef t) = show t ++ "#"
    show (TFun t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"

(~) :: LuminaType -> LuminaType -> Bool
TAny ~ _ = True
_ ~ TAny = True
TInt ~ TInt = True
TBool ~ TBool = True
TUnit ~ TUnit = True
TRef t1 ~ TRef t2 = t1 ~ t2
TFun t1 t1' ~ TFun t2 t2' = t1 ~ t2 && t1' ~ t2'
_ ~ _ = False


getReturnType :: LuminaType -> LuminaType
getReturnType (TFun _ t) = t
getReturnType tt = internalError $ "Tried to get the return type of " ++ show tt ++ ", which is not a function type."