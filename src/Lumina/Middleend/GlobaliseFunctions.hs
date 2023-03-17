{-# LANGUAGE LambdaCase #-}
module Lumina.Middleend.GlobaliseFunctions (globaliseFunctions, globalToLambda) where

import Lumina.Frontend.LuminaAST (AST (..), (>>:=), freeVars, replaceVar)

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Trans.State (State, get, put)
import Lumina.Middleend.EliminateEtaRedex (elimEtaGlobal)

-- GlobalisedFunction is of the form (args in reverse order, expression with no inner functions)
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
        put (i+1, Map.insert newName (elimEtaGlobal (x : reverse fv, ast1')) env)

        let replacement = foldl (\a y -> AApp a (AVar y)) (AVar newName) fv
        return $ replaceVar f replacement ast2'
    ast -> globaliseFunctions >>:= ast

globalToLambda :: GlobalisedFunction -> AST
globalToLambda ([], ast) = ast
globalToLambda (x:xs, ast) = globalToLambda (xs, AFun x ast)