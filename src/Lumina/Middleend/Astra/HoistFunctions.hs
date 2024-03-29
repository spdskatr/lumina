{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Lumina.Middleend.Astra.HoistFunctions (TypedVar (..), GlobalisedFunction, FunctionEnv, globaliseFunctions) where

import Lumina.Middleend.Astra.Astra (AST (..), (>>:=), replaceVar, (><>), astraType)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Bifunctor as Bifunctor
import Control.Monad.Trans.State.Strict (State, state, runState, modify)
import Lumina.Utils (fastNub, untilFixedPoint, internalError)
import Lumina.Middleend.Typing (LuminaType(..))
import Data.Function (on)

data TypedVar = TypedVar {
        getVarName :: String,
        getVarType :: LuminaType 
    } deriving Eq
instance Ord TypedVar where
    compare = compare `on` getVarName
instance Show TypedVar where
    show (TypedVar n t) = n ++ " : " ++ show t

-- GlobalisedFunction is of the form (captured variables, expression with no inner functions)
type GlobalisedFunction = ([TypedVar], AST)
type FunctionEnv = Map String GlobalisedFunction
type GlobaliseState a = State (Int, FunctionEnv) a

globalName :: String -> State (Int, FunctionEnv) String
globalName name = state $ \(i, env) ->
    (show i ++ "_" ++ name, (i+1, env))

newGlobalFunc :: String -> GlobalisedFunction -> GlobaliseState ()
newGlobalFunc name fd = modify $ Bifunctor.second $ Map.insert name fd

allFreeVars :: FunctionEnv -> AST -> [TypedVar]
allFreeVars env ast =
    case ast of
        AVar x t -> case Map.lookup x env of
            Nothing -> [TypedVar x t]
            Just (fv, _) -> fv
        AFun x _ ast1 _ -> filter (\(TypedVar y _) -> x /= y) $ allFreeVars env ast1
        ALet x _ ast1 ast2 _ ->
            let fv1 = allFreeVars env ast1
                fv2 = filter (\(TypedVar y _) -> x /= y) $ allFreeVars env ast2
            in fastNub $ fv1 ++ fv2
        ALetFun f x _ ast1 ast2 _ -> 
            let fv1 = filter (\(TypedVar y _) -> y /= x && y /= f) $ allFreeVars env ast1
                fv2 = filter (\(TypedVar y _) -> y /= f) $ allFreeVars env ast2
            in fastNub $ fv1 ++ fv2
        _ -> fastNub $ allFreeVars env ><> ast

globaliseFunctions :: AST -> FunctionEnv
globaliseFunctions ast = 
    let (a, (_, env)) = runState (globaliseFunctionsImpl ast) (0, Map.empty)
        newEnv = Map.insert "0main" ([], AFun "0arg" TUnit a (TFun TUnit (astraType a))) env
    in untilFixedPoint populateFreeVars newEnv

populateFreeVars :: FunctionEnv -> FunctionEnv
populateFreeVars env = Map.map changeFreeVars env
    where
        changeFreeVars (_, ast) = (allFreeVars env ast, ast)

globaliseFunctionsImpl :: AST -> GlobaliseState AST
globaliseFunctionsImpl = \case
    AFun s t ast t' -> globaliseFunctionsImpl (ALetFun "lambda" s t ast (AVar "lambda" hole) t')
    ALetFun f x t ast1 ast2 _ -> do
        let ft = TFun t (astraType ast1)
        newName <- globalName f
        -- Insert a placeholder, to tell recursive calls that the name will exist
        newGlobalFunc newName ([], AVar newName ft)
        ast1' <- globaliseFunctionsImpl $ replaceVar f (AVar newName ft) ast1
        ast2' <- globaliseFunctionsImpl $ replaceVar f (AVar newName ft) ast2

        -- Insert the actual function body
        newGlobalFunc newName ([], AFun x t ast1' ft)

        -- Output the rest
        return ast2'
    ast -> globaliseFunctionsImpl >>:= ast
    where
        hole = internalError "Attempted to evaluate unfilled hole - this should never happen"