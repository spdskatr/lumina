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
    AVar x t -> case env Map.!? x of
        Just s -> Just $ AVar s t
        Nothing -> unboundVarError x
    AFun x t ast t' -> 
        let newEnv = insertVar x x env
        in Just $ AFun (newEnv Map.! x) t (elimShadowingImpl newEnv >:= ast) t'
    ALet x t a1 a2 t' ->
        let newEnv = insertVar x x env
        in Just $ ALet (newEnv Map.! x) t (elimShadowingImpl env >:= a1) (elimShadowingImpl newEnv >:= a2) t'
    ALetFun f x t a1 a2 t' ->
        let fEnv = insertVar f f env
            xEnv = insertVar x x fEnv
        in Just $ ALetFun (fEnv Map.! f) (xEnv Map.! x) t (elimShadowingImpl xEnv >:= a1) (elimShadowingImpl fEnv >:= a2) t'
    _ -> Nothing

insertVar :: String -> String -> Map String String -> Map String String
insertVar x y env = case env Map.!? y of
    Nothing -> Map.insert x y (Map.insert y y env)
    Just _ -> insertVar x ('_':y) env