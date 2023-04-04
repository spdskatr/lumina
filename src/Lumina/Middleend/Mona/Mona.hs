module Lumina.Middleend.Mona.Mona (MAtom (..), MValue (..), MExpr (..), MonaFunction(..), (>:=), toMonadicForm, astraToMona) where

import Lumina.Middleend.Astra.Astra (UnaryOp, BinaryOp, AST (..), astraType)
import Lumina.Utils (internalError, indent, orElse)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Lumina.Middleend.Astra.HoistFunctions (globaliseFunctions)
import Data.List (intercalate)
import Lumina.Middleend.Typing (LuminaType (..))
import Lumina.Middleend.Astra.ElimShadowing (elimShadowing)

{-
 - After hoisting all of our functions into a main environment and converting
 - to CPS, our expressions are actually very close to Administrative Normal Form
 - (ANF). Our job is hence to "officially" translate the AST to ANF.
 - 
 - ANF is also known as Monadic Normal Form. From now on I will simply refer to
 - it as "monadic form."
 -
 - Note that apart from if statements, this IR is almost entirely linear.
 - 
 - In an ideal world I would have gone directly to this form but I have to demo
 - CPS form first.
 -}

data MonaFunction = MonaFunction 
    { getName :: String
    , getFV :: [String]
    , getArg :: String
    , getBody :: MExpr 
    }

instance Show MonaFunction where
    show (MonaFunction f fv x e) = 
        "define " ++ f ++ "[" ++ intercalate ", " fv ++ "](" ++ x ++ ") =\n" ++ indent (show e)

data MAtom
    = MVar String
    | MInt Int
    | MBool Bool
    | MUnit
    deriving Eq

instance Show MAtom where
    show (MVar s) = s
    show (MInt n) = show n
    show (MBool b) = show b
    show MUnit = show ()

data MValue
    = MJust MAtom
    | MUnary UnaryOp MAtom
    | MBinary BinaryOp MAtom MAtom
    | MApp MAtom MAtom
    deriving Eq

instance Show MValue where
    show (MJust val) = show val
    show (MUnary uo val) = show uo ++ show val
    show (MBinary bo v1 v2) = "(" ++ show v1 ++ " " ++ show bo ++ " " ++ show v2 ++ ")"
    show (MApp v1 v2) = show v1 ++ " " ++ show v2

data MExpr
    = MLet String LuminaType MValue MExpr
    | MAssign MAtom MAtom MExpr
    | MIf MAtom MExpr MExpr
    | MReturn MAtom
    deriving Eq

instance Show MExpr where
    show (MLet s t v ex) = "let " ++ s ++ " : " ++ show t ++ " = " ++ show v ++ "\n" ++ show ex
    show (MAssign v1 v2 ex) = "set " ++ show v1 ++ " := " ++ show v2 ++ "\n" ++ show ex
    show (MIf v e1 e2) = "if " ++ show v ++ " then\n" ++ indent (show e1) ++ "else\n" ++ indent (show e2)
    show (MReturn v) = "return " ++ show v

(>:=) :: (MExpr -> Maybe MExpr) -> MExpr -> MExpr
f >:= m = f m `orElse` recurse
    where
        recurse = case m of
            MLet s t mv me -> MLet s t mv (f >:= me)
            MAssign ma ma' me -> MAssign ma ma' me
            MIf ma me me' -> MIf ma (f >:= me) (f >:= me')
            MReturn ma -> MReturn ma

monadicFormError :: String -> a
monadicFormError s = internalError ("Could not convert to monadic form: " ++ s)

-- Takes Astra in Continuation Form and converts it to Mona.
toMonadicForm :: AST -> MExpr
toMonadicForm ast = monadicFormError "not implemented"

astraToMona :: AST -> Map String MonaFunction
astraToMona ast = Map.mapWithKey translateChunk $ globaliseFunctions $ elimShadowing ast
    where
        translateChunk name (fv,a) = case a of
            (AFun x _ a' _) -> MonaFunction { getName = name, getFV = fv, getArg = x, getBody = toMonadicForm a' }
            _ -> internalError $ "Could not translate to Mona function because expression is not a function:\n" ++ show a