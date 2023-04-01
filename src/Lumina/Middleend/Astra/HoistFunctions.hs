{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Lumina.Middleend.Astra.HoistFunctions (GlobalisedFunction, FunctionEnv, globaliseFunctions, toContinuationForm) where

import Lumina.Middleend.Astra.Astra (AST (..), (>>:=), replaceVar, (><>))
import Lumina.Middleend.Astra.CPS (cps)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Bifunctor as Bifunctor
import Control.Monad.Trans.State.Strict (State, state, runState, modify)
import Lumina.Utils (fastNub, untilFixedPoint)

-- GlobalisedFunction is of the form (captured variables, expression with no inner functions)
type GlobalisedFunction = ([String], AST)
type FunctionEnv = Map String GlobalisedFunction
type GlobaliseState a = State (Int, FunctionEnv) a

globalName :: String -> State (Int, FunctionEnv) String
globalName name = state $ \(i, env) ->
    (show i ++ "_" ++ name, (i+1, env))

newGlobalFunc :: String -> GlobalisedFunction -> GlobaliseState ()
newGlobalFunc name fd = modify $ Bifunctor.second $ Map.insert name fd

allFreeVars :: FunctionEnv -> AST -> [String]
allFreeVars env ast =
    case ast of
        AVar x -> case Map.lookup x env of
            Nothing -> [x]
            Just (fv, _) -> fv
        AFun x ast1 -> filter (/= x) $ allFreeVars env ast1
        ALet x ast1 ast2 ->
            let fv1 = allFreeVars env ast1
                fv2 = filter (/= x) $ allFreeVars env ast2
            in fastNub $ fv1 ++ fv2
        ALetFun f x ast1 ast2 -> 
            let fv1 = filter (\y -> y /= x && y /= f) $ allFreeVars env ast1
                fv2 = filter (/= f) $ allFreeVars env ast2
            in fastNub $ fv1 ++ fv2
        _ -> fastNub $ allFreeVars env ><> ast

globaliseFunctions :: AST -> FunctionEnv
globaliseFunctions ast = 
    let (a, (_, env)) = runState (globaliseFunctionsImpl ast) (0, Map.empty)
        newEnv = Map.insert "0main" ([], AFun "0arg" a) env
    in untilFixedPoint populateFreeVars newEnv

populateFreeVars :: FunctionEnv -> FunctionEnv
populateFreeVars env = Map.map changeFreeVars env
    where
        changeFreeVars (_, ast) = (allFreeVars env ast, ast)

globaliseFunctionsImpl :: AST -> GlobaliseState AST
globaliseFunctionsImpl = \case
    AFun s ast -> globaliseFunctionsImpl (ALetFun "lambda" s ast (AVar "lambda"))
    ALetFun f x ast1 ast2 -> do
        newName <- globalName f
        -- Insert a placeholder, to tell recursive calls that the name will exist
        newGlobalFunc newName ([], AVar newName)
        ast1' <- globaliseFunctionsImpl $ replaceVar f (AVar newName) ast1
        ast2' <- globaliseFunctionsImpl $ replaceVar f (AVar newName) ast2

        -- Insert the actual function body
        newGlobalFunc newName ([], AFun x ast1')

        -- Output the rest
        return ast2'
    ast -> globaliseFunctionsImpl >>:= ast

allToCPS :: (Int, FunctionEnv) -> String -> GlobalisedFunction -> (Int, FunctionEnv)
allToCPS (i, env) f (fv, ast) =
    let (ast', j) = runState (cps ast return) i
    in (j, Map.insert f (fv, ast') env)

{- "Continuation form" is an intermediate representation I made up in which 
 - every non-continuation function has a name (i.e. the only lambdas are 
 - continuations) and the body of every function is in continuation-passing
 - style.
 -}
toContinuationForm :: AST -> FunctionEnv
toContinuationForm ast =
    let env = globaliseFunctions ast
    in snd $ Map.foldlWithKey allToCPS (0, Map.empty) env
