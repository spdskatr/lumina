module Lumina.Middleend.Typing (LuminaType(..), getReturnType) where

import Lumina.Utils (internalError)

data LuminaType
    = TInt
    | TBool
    | TUnit
    | TRef LuminaType
    | TFun LuminaType LuminaType
    deriving Eq

instance Show LuminaType where
    show TInt = "int"
    show TBool = "bool"
    show TUnit = "unit"
    show (TRef t) = show t ++ "#"
    show (TFun t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"


getReturnType :: LuminaType -> LuminaType
getReturnType (TFun _ t) = t
getReturnType tt = internalError $ "Tried to get the return type of " ++ show tt ++ ", which is not a function type."