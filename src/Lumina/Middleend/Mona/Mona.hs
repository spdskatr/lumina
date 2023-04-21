module Lumina.Middleend.Mona.Mona (MAtom (..), MOper (..), MExpr (..), MonaFunction(..), MonaTranslationUnit, (>:=), astraToMona) where

import Lumina.Middleend.Astra.Astra (UnaryOp (..), BinaryOp (..), AST (..))
import Lumina.Utils (internalError, indent, orElse)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Lumina.Middleend.Astra.HoistFunctions (globaliseFunctions, TypedVar (..), FunctionEnv)
import Data.List (intercalate)
import Lumina.Middleend.Typing (LuminaType (..), getReturnType)
import Lumina.Middleend.Astra.ElimShadowing (elimShadowing)

{-
 - Mona is an IR that represents a functional program in Administrative Normal
 - Form (ANF), sometimes also called A-normal form and Monadic Normal Form.
 - The IR consists of let-bindings, if-statements, explicit side-effects
 - (either through ref assignment or a function call) and no inner functions.
 -
 - Types are still kept in this form.
 - 
 - It is essentially the functional equivalent of Static Single-Assigmnent
 - (SSA) form.
 - 
 - The bulk of the optimisations are performed in this IR.
 -}

data MonaFunction = MonaFunction
    { getName :: String
    , getFV :: [TypedVar]
    , getArg :: String
    , getArgType :: LuminaType
    , getResultType :: LuminaType
    , getBody :: MExpr
    } deriving Eq

type MonaTranslationUnit = Map String MonaFunction

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

data MOper
    = MJust MAtom
    | MMkClosure String [(String, MAtom)]
    | MUnary UnaryOp MAtom
    | MBinary BinaryOp MAtom MAtom
    | MApp MAtom MAtom
    | MCall String [(String, MAtom)] MAtom
    deriving Eq

instance Show MOper where
    show (MJust val) = show val
    show (MMkClosure cl env) = "MKCL " ++ show cl ++ " " ++ show env
    show (MUnary uo val) = show uo ++ show val
    show (MBinary bo v1 v2) = "(" ++ show v1 ++ " " ++ show bo ++ " " ++ show v2 ++ ")"
    show (MApp v1 v2) = "APPLY " ++ show v1 ++ " " ++ show v2
    show (MCall s args v) = s ++ "[" ++ intercalate "," (map show args) ++ "](" ++ show v ++ ")"

data MExpr
    = MLet String LuminaType MOper MExpr
    | MAssign MAtom MAtom MExpr
    | MIf MAtom MExpr MExpr
    | MLetInline String LuminaType MExpr MExpr
    | MReturn MAtom
    deriving Eq

instance Show MExpr where
    show (MLet s t v ex) = "let " ++ s ++ " : " ++ show t ++ " = " ++ show v ++ "\n" ++ show ex
    show (MAssign v1 v2 ex) = "set " ++ show v1 ++ " := " ++ show v2 ++ "\n" ++ show ex
    show (MIf v e1 e2) = "if " ++ show v ++ " then\n" ++ indent (show e1) ++ "else\n" ++ indent (show e2)
    show (MLetInline s t ex nx) = "let " ++ s ++ " : " ++ show t ++ " =\n" ++ indent (show ex) ++ show nx
    show (MReturn v) = "return " ++ show v

(>:=) :: (MExpr -> Maybe MExpr) -> MExpr -> MExpr
f >:= m = f m `orElse` recurse
    where
        recurse = case m of
            MLet s t mv me -> MLet s t mv (f >:= me)
            MAssign ma ma' me -> MAssign ma ma' me
            MIf ma me me' -> MIf ma (f >:= me) (f >:= me') 
            MLetInline s t ex nx -> MLetInline s t (f >:= ex) (f >:= nx)
            MReturn ma -> MReturn ma

monadicFormError :: String -> a
monadicFormError s = internalError ("Could not convert to monadic form: " ++ s)

-- Takes Astra and converts it to Mona. We whizz past continuation-passing
-- style along the way.
toMona :: FunctionEnv -> AST -> (MAtom -> State Int MExpr) -> State Int MExpr
toMona fs ast k = case ast of
    ABool b -> k (MBool b)
    AInt n -> k (MInt n)
    AUnit -> k MUnit
    AVar _ TUnit -> k MUnit
    AVar s t -> 
        case fs Map.!? s of
            Just (vs, _) -> newLet (MMkClosure s [(x, MVar x) | (TypedVar x _) <- vs]) t
            Nothing -> k (MVar s)
    AApp a1 a2 t -> toMona fs a1 (\a1' -> toMona fs a2 (\a2' -> newLet (MApp a1' a2') t))
    AUnaryOp uo a t -> toMona fs a $ \a' ->
        newLet (MUnary uo a') t
    ABinaryOp OpAnd a1 a2 t -> toMona fs a1 $ \a1' -> do
        newLetIf t a1' a2 (ABool False)
    ABinaryOp OpOr a1 a2 t -> toMona fs a1 $ \a1' -> do
        newLetIf t a1' (ABool True) a2
    ABinaryOp bo a1 a2 _ -> toMona fs a1 $ \a1' -> toMona fs a2 $ \a2' ->
        let tres = case bo of
              OpAdd -> TInt
              OpSub -> TInt
              OpMul -> TInt
              OpLessThan -> TBool
              OpIntEqual -> TBool
              OpBoolEqual -> TBool
        in newLet (MBinary bo a1' a2') tres
    AAssign a1 a2 -> toMona fs a1 $ \a1' -> toMona fs a2 $ \a2' -> MAssign a1' a2' <$> k MUnit
    AIf c a1 a2 t -> toMona fs c $ \c' -> do
        newLetIf t c' a1 a2
    ALet s t a1 a2 _ -> toMona fs a1 $ \a1' -> MLet s t (MJust a1') <$> toMona fs a2 k
    ASeq a1 a2 _ -> toMona fs a1 $ \_ -> toMona fs a2 k
    AFun {} -> monadicFormError $ "Encountered lambda when trying to convert to Mona (this should never happen): " ++ show ast
    ALetFun {} -> monadicFormError $ "Encountered inner function definition when trying to convert to Mona (this should never happen): " ++ show ast
    where
        newLetIf t cond th el = do
            i <- get
            put (i+1)
            let ident = show i ++ "phi"
            nx <- k (MVar ident)
            m1 <- toMona fs th (return . MReturn)
            m2 <- toMona fs el (return . MReturn)
            return $ MLetInline ident t (MIf cond m1 m2) nx
        newLet v t = do
            i <- get
            put (i+1)
            let ident = show i ++ "v"
            nx <- k (MVar ident)
            return $ MLet ident t v nx

astraToMona :: AST -> MonaTranslationUnit
astraToMona ast = 
    let fenv = globaliseFunctions $ elimShadowing ast
        translateChunk name (fv,a) = case a of
            (AFun x t a' t') -> MonaFunction name fv x t (getReturnType t') (evalState (toMona fenv a' (return . MReturn)) 0)
            _ -> internalError $ "Could not translate to Mona function because expression is not a function:\n" ++ show a
    in Map.mapWithKey translateChunk fenv