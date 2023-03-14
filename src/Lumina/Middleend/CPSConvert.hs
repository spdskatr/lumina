module Lumina.Middleend.CPSConvert (toCPS) where

{-
 - Converts code to continutation passing style, eliminating all tail recursion
 - and introducing a whole load of lambdas.
 - 
 - Note that this code still produces a few eta-redexes (terms of the form 
 - "fun x -> f x end"), which I will eliminate to just "f" in the next step.
 -}

import Lumina.Frontend.LuminaAST (AST(..))

import Control.Monad.Trans.State (State, get, put, evalState)

type CPSTable a = State Int a

newVar :: State Int String
newVar = do
    i <- get
    put (i+1)
    return $ "0k" ++ show i

liftCont :: (AST -> CPSTable AST) -> CPSTable AST
liftCont k = do
    r <- newVar
    inner <- k (AVar r)
    return $ AFun r inner

cps :: AST -> (AST -> CPSTable AST) -> CPSTable AST
cps ast k = case ast of
    ABool b -> k (ABool b)
    AInt n -> k (AInt n)
    AUnit -> k AUnit
    AVar s -> k (AVar s)
    AApp ast' ast2 -> do
        cont <- liftCont k
        cps ast' (\f -> cps ast2 (\a -> return (AApp (AApp f a) cont)))
    AUnaryOp uo ast' -> cps ast' (k . AUnaryOp uo)
    ABinaryOp bo ast' ast2 -> cps ast' (\a -> cps ast2 (k . ABinaryOp bo a))
    AAssign ast' ast2 -> cps ast' (\a -> cps ast2 (k . AAssign a))
    AIf acond athen aelse -> cps acond $ \a -> do
        c <- liftCont k
        cpsthen <- cps athen (return . AApp c)
        cpselse <- cps aelse (return . AApp c)
        return (AIf a cpsthen cpselse)
    AEmptyCase -> k AEmptyCase
    AFun s ast' -> do
        c <- newVar
        res <- cps ast' (return . AApp (AVar c))
        k (AFun s (AFun c res))
    ALetFun f x ast' ast2 -> do
        c <- newVar
        res <- cps ast' (return . AApp (AVar c))
        outer <- cps ast2 k
        return (ALetFun f x (AFun c res) outer)
    AWhile ast' ast2 -> 
        cps ast' $ \r -> do
            body <- cps ast2 k
            return (AWhile r body)
    ASeq ast' ast2 -> cps ast' (\_ -> cps ast2 k)

toCPS :: AST -> AST
toCPS ast = evalState (cps ast return) 0