{-# LANGUAGE LambdaCase #-}
module Lumina.Middleend.EliminateEtaRedex (fullyElimEta) where
import Lumina.Frontend.LuminaAST (AST (..), freeVars, (>:=))
import Lumina.Utils (untilFixedPoint)

-- Recursively pattern match for an eta-redex in the code.
elimEta :: AST -> AST
elimEta ast = elimEtaImpl >:= ast
    where
        elimEtaImpl = \case
            AFun x (AApp ast' (AVar y)) | x == y && x `notElem` freeVars ast' ->
                Just $ elimEta ast'
            _ -> Nothing

fullyElimEta :: AST -> AST
fullyElimEta = untilFixedPoint elimEta