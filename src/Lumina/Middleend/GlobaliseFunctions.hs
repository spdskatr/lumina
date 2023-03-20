{-# LANGUAGE LambdaCase #-}
module Lumina.Middleend.GlobaliseFunctions (GlobalisedFunction, FunctionEnv, globaliseFunctions, toContinuationForm) where

import Lumina.Frontend.LuminaAST (AST (..), (>>:=), freeVars, replaceVar)
import Lumina.Middleend.CPSConvert (toCPS)

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Bifunctor as Bifunctor
import Control.Monad.Trans.State (State, get, put, runState)

-- GlobalisedFunction is of the form (argument : captured variables, expression with no inner functions)
type GlobalisedFunction = ([String], AST)
type FunctionEnv = Map String GlobalisedFunction

newFunctionName :: Int -> String -> String
newFunctionName i s = "0_" ++ s ++ "_" ++ show i

globaliseFunctions :: AST -> State (Int, FunctionEnv) AST
globaliseFunctions = \case
    AFun s ast -> globaliseFunctions (ALetFun "lambda" s ast (AVar "lambda"))
    ALetFun f x ast1 ast2 -> do
        ast1' <- globaliseFunctions ast1
        ast2' <- globaliseFunctions ast2

        (i, env) <- get
        let fv = filter (`Map.notMember` env) $ freeVars (ALetFun f x ast1' (AVar f))
        let newName = newFunctionName i f
        put (i+1, Map.insert newName (x : fv, ast1') env)

        --let replacement = foldl (\a y -> AApp a (AVar y)) (AVar newName) fv
        let replacement = AVar newName
        return $ replaceVar f replacement ast2'
    ast -> globaliseFunctions >>:= ast

{- "Continuation form" is an intermediate representation I made up in which 
 - every non-continuation function has a name (i.e. the only lambdas are 
 - continuations) and the body of every function is in continuation-passing
 - style.
 -}
toContinuationForm :: AST -> FunctionEnv
toContinuationForm ast =
    let (a, (_, env)) = runState (globaliseFunctions ast) (0, Map.empty)
        newEnv = Map.insert "0main" (["_"], a) env
    in Map.map (Bifunctor.second toCPS) newEnv