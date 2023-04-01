{-# LANGUAGE LambdaCase #-}
module Lumina.Middleend.RedexElimination (elimEta, elimTrivialBeta) where
import Lumina.Frontend.LuminaAST (AST (..), freeVars, (>:=), isTrivial, replaceVar)

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

-- Recursively pattern match for a trivial beta-redex in the code.
-- A beta-redex is trivial (i made up the term) if the argument is an atom
-- such as an integer or variable.
elimTrivialBeta :: AST -> AST
elimTrivialBeta ast = elimTrivialBetaImpl >:= ast
    where
        elimTrivialBetaImpl = \case
            AApp (AFun x a) b | isTrivial b -> Just (elimTrivialBeta $ replaceVar x b a)
            _ -> Nothing