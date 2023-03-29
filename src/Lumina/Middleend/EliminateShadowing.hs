{-# LANGUAGE LambdaCase #-}
module Lumina.Middleend.EliminateShadowing (elimShadowing) where

import Lumina.Frontend.LuminaAST (AST (..), (>:=))

import Data.Map (Map)
import qualified Data.Map as Map
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
    ALetFun f x a1 a2 ->
        let fEnv = insertVar f f env
            xEnv = insertVar x x fEnv
        in Just $ ALetFun (fEnv Map.! f) (xEnv Map.! x) (elimShadowingImpl xEnv >:= a1) (elimShadowingImpl fEnv >:= a2)
    _ -> Nothing

insertVar :: String -> String -> Map String String -> Map String String
insertVar x y env = case env Map.!? y of
    Nothing -> Map.insert x y (Map.insert y y env)
    Just _ -> insertVar x ('_':y) env