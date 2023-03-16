{-# LANGUAGE LambdaCase #-}
module Lumina.Middleend.EliminateEtaRedex (fullyElimEta, elimEtaGlobal) where
import Lumina.Frontend.LuminaAST (AST (..), freeVars, (>:=))
import Lumina.Utils (untilFixedPoint)

-- Recursively pattern match for an eta-redex in the code.
-- NOTE: I know that in the most general case, code that is not in CPS form
-- will not be semantically equivalent under eta eliminating transformations.
-- In CPS form, since the evaluation order is made explicit, evaluation order
-- is kept.
elimEta :: AST -> AST
elimEta ast = elimEtaImpl >:= ast
    where
        elimEtaImpl = \case
            AFun x (AApp ast' (AVar y)) | x == y && x `notElem` freeVars ast' ->
                Just $ elimEta ast'
            _ -> Nothing

-- Second pass of eta-reduction this time on globalised functions, used in GlobaliseFunctions
elimEtaGlobal :: ([String], AST) -> ([String], AST)
elimEtaGlobal (x:xs, AApp b (AVar y)) = if x == y then elimEtaGlobal (xs,b) else (x:xs, AApp b (AVar y))
elimEtaGlobal args = args

fullyElimEta :: AST -> AST
fullyElimEta = untilFixedPoint elimEta