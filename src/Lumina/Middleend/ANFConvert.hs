module Lumina.Middleend.ANFConvert (MContEnv, MValue, MExpr, toMonadicForm) where

import Lumina.Frontend.LuminaAST (UnaryOp, BinaryOp, AST (..))
import Lumina.Utils (internalError, indent)

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Trans.State.Strict (State, evalState, get, put)

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
    = ContInline String AST
    | ContReturn

type MContEnv = Map String ContType
type MVarEnv = Map String MAtom

data MAtom
    = MVar String
    | MInt Int
    | MBool Bool
    | MUnit

instance Show MAtom where
    show (MVar s) = s
    show (MInt n) = show n
    show (MBool b) = show b
    show MUnit = show ()

data MValue
    = MUnary UnaryOp MAtom
    | MBinary BinaryOp MAtom MAtom
    | MApp MAtom MAtom
    | MAssign MAtom MAtom

instance Show MValue where
    show (MUnary uo val) = show uo ++ show val
    show (MBinary bo v1 v2) = "(" ++ show v1 ++ " " ++ show bo ++ " " ++ show v2 ++ ")"
    show (MApp v1 v2) = show v1 ++ " " ++ show v2
    show (MAssign v1 v2) = show v1 ++ " := " ++ show v2

data MExpr
    = MLet String MValue MExpr
    | MIf MAtom MExpr MExpr
    | MReturn MAtom

instance Show MExpr where
    show (MLet s v ex) = "let " ++ s ++ " = " ++ show v ++ "\n" ++ show ex
    show (MIf v e1 e2) = "if " ++ show v ++ " then\n" ++ indent (show e1) ++ "else\n" ++ indent (show e2)
    show (MReturn v) = "return " ++ show v

monadicFormError :: String -> a
monadicFormError s = internalError ("Could not convert to monadic form: " ++ s)

getMValue :: AST -> (MAtom -> State Int MExpr) -> State Int MExpr
getMValue a k = case a of
    ABool b -> k (MBool b)
    AInt n -> k (MInt n)
    AUnit -> k MUnit
    AVar s -> k (MVar s)
    AUnaryOp uo ast -> do
        t <- tmpVar
        getMValue ast (\r -> MLet t (MUnary uo r) <$> k (MVar t))
    ABinaryOp bo ast1 ast2 -> do
        t <- tmpVar
        getMValue ast1 (\r -> getMValue ast2 (\s -> MLet t (MBinary bo r s) <$> k (MVar t)))
    AAssign ast1 ast2 -> do
        t <- tmpVar
        getMValue ast1 (\r -> getMValue ast2 (\s -> MLet t (MAssign r s) <$> k (MVar t)))
    _ -> monadicFormError ("Could got get MValue - AST contains function/control nodes: " ++ show a)
    where
        tmpVar = do
            i <- get
            put (i+1)
            return (show i ++ "val")

toMonadicFormImpl :: MContEnv -> MVarEnv -> AST -> State Int MExpr
toMonadicFormImpl env ve a = case a of
    -- Values
    ABool _ -> trivial
    AInt _ -> trivial
    AUnit -> trivial
    AVar _ -> trivial
    AUnaryOp {} -> trivial
    ABinaryOp {} -> trivial
    AAssign {} -> trivial
    -- Cont continuations
    AApp (AApp a1 a2) (AFun x a3) ->
        getMValue a1 (\p -> getMValue a2 (\q -> MLet x (MApp p q) <$> recOn a3))
    AApp (AApp a1 a2) (AVar k) -> case env Map.!? k of
        Just (ContInline x a3) -> recOn (AApp (AApp a1 a2) (AFun x a3))
        Just ContReturn ->
            getMValue a1 (\p -> getMValue a2 (\q -> return $ MLet "0ret" (MApp p q) (MReturn (MVar "0ret"))))
        Nothing -> monadicFormError ("could not find continuation: " ++ show k)
    -- Jump continuations
    AApp (AFun x a1) (AFun y a2) ->
        recWithCont x (ContInline y a2) a1
    AApp (AFun x a1) (AVar k) -> case env Map.!? k of
        Just p -> recWithCont x p a1
        Nothing -> recReplace x (MVar k) a1
    AApp (AFun x a1) a2 -> getMValue a2 (\p -> recReplace x p a1)
    -- Environment continuations
    AApp (AVar k) ast -> case env Map.!? k of
        Just (ContInline x a1) -> recOn (AApp (AFun x a1) ast)
        Just ContReturn -> getMValue ast (return . MReturn)
        Nothing -> monadicFormError ("could not find continuation: " ++ show k)
    -- If statements
    AIf ast ast' ast2 -> getMValue ast (\p -> MIf p <$> recOn ast' <*> recOn ast2)
    _ -> monadicFormError ("encountered invalid expression: " ++ show a)
    where
        trivial = getMValue a (return . MReturn)
        recOn = toMonadicFormImpl env ve
        recWithCont x k = toMonadicFormImpl (Map.insert x k env) ve
        recReplace x b = toMonadicFormImpl env (Map.insert x b ve)

toMonadicForm :: String -> AST -> MExpr
toMonadicForm k ast = evalState (toMonadicFormImpl (Map.singleton k ContReturn) Map.empty ast) 0