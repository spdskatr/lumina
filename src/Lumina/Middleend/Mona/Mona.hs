module Lumina.Middleend.Mona.Mona (MContEnv, MAtom (..), MValue (..), MExpr (..), MonaFunction(..), (>:=), toMonadicForm, astraToMona) where

import Lumina.Middleend.Astra.Astra (UnaryOp, BinaryOp, AST (..))
import Lumina.Utils (internalError, indent, orElse)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Lumina.Middleend.Astra.HoistFunctions (toContinuationForm)
import Data.List (intercalate)
import Lumina.Middleend.Typing (LuminaType (..))

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

data ContType
    = ContInline String LuminaType AST LuminaType
    | ContReturn

type MContEnv = Map String ContType

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

getMValue :: AST -> (MAtom -> State Int MExpr) -> State Int MExpr
getMValue a k = case a of
    ABool b -> k (MBool b)
    AInt n -> k (MInt n)
    AUnit -> k MUnit
    AVar s _ -> k (MVar s)
    AUnaryOp uo ast t -> do
        v <- tmpVar
        getMValue ast (\r -> MLet v t (MUnary uo r) <$> k (MVar v))
    ABinaryOp bo ast1 ast2 t -> do
        v <- tmpVar
        getMValue ast1 (\r -> getMValue ast2 (\s -> MLet v t (MBinary bo r s) <$> k (MVar v)))
    AAssign ast1 ast2 -> do
        v <- tmpVar
        getMValue ast1 (\r -> getMValue ast2 (\s -> MAssign r s . MLet v TUnit (MJust MUnit) <$> k (MVar v)))
    _ -> monadicFormError ("Could got get MValue - AST contains function/control nodes: " ++ show a)
    where
        tmpVar = do
            i <- get
            put (i+1)
            return (show i ++ "val")

toMonadicFormImpl :: MContEnv -> AST -> State Int MExpr
toMonadicFormImpl env a = case a of
    -- Values
    ABool _ -> trivial
    AInt _ -> trivial
    AUnit -> trivial
    AVar _ _ -> trivial
    AUnaryOp {} -> trivial
    ABinaryOp {} -> trivial
    AAssign {} -> trivial
    -- Cont continuations
    AApp (AApp a1 a2 _) (AFun x t a3 _) _ ->
        getMValue a1 (\p -> getMValue a2 (\q -> MLet x t (MApp p q) <$> recOn a3))
    AApp (AApp a1 a2 t1) (AVar k _) t2 -> case env Map.!? k of
        Just (ContInline x t a3 t') -> recOn (AApp (AApp a1 a2 t1) (AFun x t a3 t') t2)
        Just ContReturn -> do
            ret <- retVar
            getMValue a1 (\p -> getMValue a2 (\q -> return $ MLet ret t2 (MApp p q) (MReturn (MVar ret))))
        Nothing -> monadicFormError ("could not find continuation: " ++ show k)
    -- Jump continuations
    AApp (AFun x _ a1 _) (AFun y t a2 t') _ ->
        recWithCont x (ContInline y t a2 t') a1
    AApp (AFun x t a1 _) (AVar k _) _ -> case env Map.!? k of
        Just p -> recWithCont x p a1
        Nothing -> MLet x t (MJust $ MVar k) <$> toMonadicFormImpl env a1
    AApp (AFun x t a1 _) a2 _ -> getMValue a2 (\p -> MLet x t (MJust p) <$> toMonadicFormImpl env a1)
    -- Environment continuations
    AApp (AVar k _) ast tres -> case env Map.!? k of
        Just (ContInline x t a1 t') -> recOn (AApp (AFun x t a1 t') ast tres)
        Just ContReturn -> getMValue ast (return . MReturn)
        Nothing -> monadicFormError ("could not find continuation: " ++ show k)
    -- If statements
    AIf ast ast' ast2 _ -> getMValue ast (\p -> MIf p <$> recOn ast' <*> recOn ast2)
    _ -> monadicFormError ("encountered invalid expression: " ++ show a)
    where
        trivial = getMValue a (return . MReturn)
        recOn = toMonadicFormImpl env
        recWithCont x k = toMonadicFormImpl (Map.insert x k env)
        retVar = do
            i <- get
            put (i+1)
            return (show i ++ "ret")

-- Takes Astra in Continuation Form and converts it to Mona.
toMonadicForm :: String -> AST -> MExpr
toMonadicForm k ast = evalState (toMonadicFormImpl (Map.singleton k ContReturn) ast) 0

astraToMona :: AST -> Map String MonaFunction
astraToMona ast = Map.mapWithKey translateChunk $ toContinuationForm ast
    where
        translateChunk name (fv,a) = case a of
            (AFun x _ (AFun k _ a' _) _) -> MonaFunction { getName = name, getFV = fv, getArg = x, getBody = toMonadicForm k a' }
            _ -> internalError $ "Could not translate to Mona because Astra was not in continuation form:\n" ++ show a