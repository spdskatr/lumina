{-# LANGUAGE LambdaCase #-}
module Lumina.Middleend.Astra.ElimShadowing (elimShadowing) where

import Lumina.Middleend.Astra.Astra (AST (..), (>:=))

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Lumina.Utils (internalError)

elimShadowing :: AST -> AST
elimShadowing = (elimShadowingImpl Map.empty >:=)

unboundVarError :: String -> a
unboundVarError x =
    internalError $ "elimShadowing encountered an unbound variable " ++ x ++ ". This should have been resolved in the AST translation phase..."

elimShadowingImpl :: Map String String -> AST -> Maybe AST
elimShadowingImpl env = \case
    AVar x -> case env Map.!? x of
        Just s -> Just $ AVar s
        Nothing -> unboundVarError x
    AFun x ast -> 
        let newEnv = insertVar x x env
        in Just $ AFun (newEnv Map.! x) $ elimShadowingImpl newEnv >:= ast
    ALet x a1 a2 ->
        let newEnv = insertVar x x env
        in Just $ ALet (newEnv Map.! x) (elimShadowingImpl env >:= a1) (elimShadowingImpl newEnv >:= a2)
    ALetFun f x a1 a2 ->
        let fEnv = insertVar f f env
            xEnv = insertVar x x fEnv
        in Just $ ALetFun (fEnv Map.! f) (xEnv Map.! x) (elimShadowingImpl xEnv >:= a1) (elimShadowingImpl fEnv >:= a2)
    _ -> Nothing

insertVar :: String -> String -> Map String String -> Map String String
insertVar x y env = case env Map.!? y of
    Nothing -> Map.insert x y (Map.insert y y env)
    Just _ -> insertVar x ('_':y) env