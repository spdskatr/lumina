module Lumina.Middleend.ANFConvert where

import Lumina.Frontend.LuminaAST (UnaryOp, BinaryOp, AST (..))
import Lumina.Utils (internalError, indent)

import Data.Map (Map)
import qualified Data.Map as Map

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

type MContEnv = Map String AST

data MValue
    = MVar String
    | MInt Int
    | MBool Bool
    | MUnit
    | MUnary UnaryOp MValue
    | MBinary BinaryOp MValue MValue
    | MAssign MValue MValue

instance Show MValue where
    show (MVar s) = s
    show (MInt n) = show n
    show (MBool b) = show b
    show MUnit = show ()
    show (MUnary uo val) = show uo ++ " " ++ show val
    show (MBinary bo v1 v2) = show v1 ++ " " ++ show bo ++ " " ++ show v2
    show (MAssign v1 v2) = show v1 ++ " := " ++ show v2

data MExpr
    = MLet String MValue MExpr
    | MLetApp String MValue MValue MExpr
    | MIf MValue MExpr MExpr
    | MReturn MValue

instance Show MExpr where
    show (MLet s v ex) = "let " ++ s ++ " = " ++ show v ++ " in\n" ++ indent (show ex)
    show (MLetApp s v1 v2 ex) = "let " ++ s ++ " = " ++ show v1 ++ " " ++ show v2 ++ " in\n" ++ indent (show ex)
    show (MIf v e1 e2) = "if " ++ show v ++ " then\n" ++ indent (show e1) ++ "else\n" ++ indent (show e2)
    show (MReturn v) = "return " ++ show v



monadicFormError :: String -> a
monadicFormError s = internalError ("Could not convert to monadic form: " ++ s)

getMValue :: AST -> MValue
getMValue a = case a of
    ABool b -> MBool b
    AInt n -> MInt n
    AUnit -> MUnit
    AVar s -> MVar s
    AUnaryOp uo ast -> MUnary uo (getMValue ast)
    ABinaryOp bo ast1 ast2 -> MBinary bo (getMValue ast1) (getMValue ast2)
    AAssign ast1 ast2 -> MAssign (getMValue ast1) (getMValue ast2)
    _ -> monadicFormError ("AST contains function/control nodes: " ++ show a)

toMonadicForm :: MContEnv -> AST -> MExpr
toMonadicForm env a = case a of
    ABool b -> MReturn (MBool b)
    AInt n -> MReturn (MInt n)
    AUnit -> MReturn MUnit
    AVar s -> MReturn (MVar s)
    -- Cont continuations
    AApp (AApp a1 a2) (AFun x a3) -> MLetApp x (getMValue a1) (getMValue a2) (toMonadicForm env a3)
    AApp (AApp a1 a2) (AVar k) -> case env Map.!? k of
        Just (AFun x a3) -> toMonadicForm env (AApp (AApp a1 a2) (AFun x a3))
        Just (AVar _) -> MLetApp "0ret" (getMValue a1) (getMValue a2) (MReturn (MVar "0ret"))
        Just x -> monadicFormError ("invalid continuation for " ++ show k ++ ": " ++ show x)
        Nothing -> monadicFormError ("could not find continuation: " ++ show k)
    -- Jump continuations
    AApp (AFun x a1) (AFun y a2) ->
        toMonadicForm (Map.insert x (AFun y a2) env) a1
    AApp (AFun x a1) (AVar k) -> case env Map.!? k of
        Just p -> toMonadicForm (Map.insert x p env) a1
        Nothing -> MLet x (MVar k) (toMonadicForm env a1)
    AApp (AFun x a1) a2 -> MLet x (getMValue a2) (toMonadicForm env a1)
    -- Environment continuation
    AApp (AVar k) ast -> case env Map.!? k of
        Just (AFun x a1) -> toMonadicForm env (AApp (AFun x a1) ast)
        Just (AVar _) -> MReturn (getMValue ast)
        Just x -> monadicFormError ("invalid continuation for " ++ show k ++ ": " ++ show x)
        Nothing -> monadicFormError ("could not find continuation: " ++ show k)
    AUnaryOp uo ast -> MReturn (MUnary uo $ getMValue ast)
    ABinaryOp bo ast ast' -> MReturn (MBinary bo (getMValue ast) (getMValue ast'))
    AAssign ast ast' -> MReturn (MAssign (getMValue ast) (getMValue ast'))
    AIf ast ast' ast2 -> MIf (getMValue ast) (toMonadicForm env ast') (toMonadicForm env ast2)
    _ -> monadicFormError ("encountered invalid expression: " ++ show a)

