module Lumina.Middleend.Mona.Mona (MAtom (..), MValue (..), MExpr (..), MonaFunction(..), (>:=), toMona, astraToMona) where

import Lumina.Middleend.Astra.Astra (UnaryOp (..), BinaryOp (..), AST (..))
import Lumina.Utils (internalError, indent, orElse)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Lumina.Middleend.Astra.HoistFunctions (globaliseFunctions, TypedVar)
import Data.List (intercalate)
import Lumina.Middleend.Typing (LuminaType (..), getReturnType)
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
    , getFV :: [TypedVar]
    , getArg :: String
    , getArgType :: LuminaType
    , getResultType :: LuminaType
    , getBody :: MExpr
    }

instance Show MonaFunction where
    show (MonaFunction f fv x t t' e) =
        "define " ++ f ++ "[" ++ intercalate ", " (map show fv) ++ "](" ++ x ++ " : " ++ show t ++ ") : " ++ show t' ++ " =\n" ++ indent (show e)

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

-- Takes Astra and converts it to Mona. We whizz past continuation-passing
-- style along the way.
toMona :: AST -> (MAtom -> State Int MExpr) -> State Int MExpr
toMona ast k = case ast of
    ABool b -> k (MBool b)
    AInt n -> k (MInt n)
    AUnit -> k MUnit
    AVar s _ -> k (MVar s)
    AApp a1 a2 t -> toMona a1 (\a1' -> toMona a2 (\a2' -> newLet (MApp a1' a2') t))
    AUnaryOp uo a t -> toMona a $ \a' ->
        let tres = case uo of
                OpNot -> TBool
                OpRef -> TRef t
                OpBang -> case t of
                    TRef t' -> t'
                    _ -> monadicFormError $ "Dereferencing value of type " ++ show t ++ ", which is not a reference"
        in newLet (MUnary uo a') tres
    ABinaryOp OpAnd a1 a2 _ -> toMona a1 $ \a1' -> do
        m2 <- toMona a2 k
        m3 <- k (MBool False)
        return $ MIf a1' m2 m3
    ABinaryOp OpOr a1 a2 _ -> toMona a1 $ \a1' -> do
        m2 <- toMona a2 k
        m1 <- k (MBool True)
        return $ MIf a1' m1 m2
    ABinaryOp bo a1 a2 _ -> toMona a1 $ \a1' -> toMona a2 $ \a2' ->
        let tres = case bo of
              OpAdd -> TInt
              OpSub -> TInt
              OpMul -> TInt
              OpLessThan -> TBool
              OpIntEqual -> TBool
              OpBoolEqual -> TBool
        in newLet (MBinary bo a1' a2') tres
    AAssign a1 a2 -> toMona a1 $ \a1' -> toMona a2 $ \a2' -> MAssign a1' a2' <$> k MUnit
    AIf c a1 a2 _ -> toMona c $ \c' -> do
        m1 <- toMona a1 k
        m2 <- toMona a2 k
        return $ MIf c' m1 m2
    ALet s t a1 a2 _ -> toMona a1 $ \a1' -> MLet s t (MJust a1') <$> toMona a2 k
    ASeq a1 a2 _ -> toMona a1 $ \_ -> toMona a2 k
    AFun {} -> monadicFormError $ "Encountered lambda when trying to convert to Mona (this should never happen): " ++ show ast
    ALetFun {} -> monadicFormError $ "Encountered inner function definition when trying to convert to Mona (this should never happen): " ++ show ast
    where
        newLet v t = do
            i <- get
            put (i+1)
            let ident = show i ++ "v"
            nx <- k (MVar ident)
            return $ MLet ident t v nx

astraToMona :: AST -> Map String MonaFunction
astraToMona ast = Map.mapWithKey translateChunk $ globaliseFunctions $ elimShadowing ast
    where
        translateChunk name (fv,a) = case a of
            (AFun x t a' t') -> MonaFunction name fv x t (getReturnType t') (evalState (toMona a' (return . MReturn)) 0)
            _ -> internalError $ "Could not translate to Mona function because expression is not a function:\n" ++ show a