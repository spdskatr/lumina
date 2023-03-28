{-# LANGUAGE LambdaCase #-}
module Lumina.Middleend.EliminateEtaRedex (elimEta, elimEtaContForm) where
import Lumina.Frontend.LuminaAST (AST (..), freeVars, (>:=))
import Lumina.Middleend.GlobaliseFunctions (FunctionEnv, allFreeVars)

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

-- Like elimEta, but respects free variables within continuation forms
elimEtaContForm :: FunctionEnv -> AST -> AST
elimEtaContForm fs ast = elimEtaImpl >:= ast
    where
        elimEtaImpl = \case
            AFun x (AApp ast' (AVar y)) | x == y && x `notElem` allFreeVars fs ast ->
                Just $ elimEta ast'
            _ -> Nothing