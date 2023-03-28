module Lumina.Middleend.CPSConvert (cps, cpsTail, toCPS) where

{-
 - Converts code to continutation passing style, eliminating all tail recursion
 - and introducing a whole load of lambdas.
 - 
 - Note that this code still produces a few eta-redexes (terms of the form 
 - "fun x -> f x end"), which I will eliminate to just "f" in the next step.
 -}

import Lumina.Frontend.LuminaAST (AST(..))

import Control.Monad.Trans.State.Strict (State, get, put, evalState)

type CPSTable a = State Int a

newVar :: State Int String
newVar = do
    i <- get
    put (i+1)
    return $ show i ++ "k"

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
        -- Note: I do not want to duplicate the k continuation as it may be large.
        -- This can sometimes produce useless redexes (like 1 becoming (\x -> x) 1)
        c <- liftCont k
        cpsthen <- cpsTail athen c
        cpselse <- cpsTail aelse c
        return (AIf a cpsthen cpselse)
    AEmptyCase -> k AEmptyCase
    AFun s ast' -> do
        c <- newVar
        res <- cpsTail ast' (AVar c)
        k (AFun s (AFun c res))
    ALetFun f x ast' ast2 -> do
        c <- newVar
        res <- cpsTail ast' (AVar c)
        outer <- cps ast2 k
        return (ALetFun f x (AFun c res) outer)
    ASeq ast' ast2 -> do
        body <- cps ast2 k
        cps ast' (\a -> return $ ASeq a body)

cpsTail :: AST -> AST -> CPSTable AST
cpsTail ast k = case ast of
    ABool b -> return (AApp k (ABool b))
    AInt n -> return (AApp k (AInt n))
    AUnit -> return (AApp k AUnit)
    AVar s -> return (AApp k (AVar s))
    AApp ast' ast2 -> cps ast' (\f -> cps ast2 (\a -> return (AApp (AApp f a) k)))
    AUnaryOp uo ast' -> cps ast' (return . AApp k . AUnaryOp uo)
    ABinaryOp bo ast' ast2 -> cps ast' (\a -> cps ast2 (return . AApp k . ABinaryOp bo a))
    AAssign ast' ast2 -> cps ast' (\a -> cps ast2 (return . AApp k . AAssign a))
    AIf acond athen aelse -> cps acond $ \a -> do
        -- Note: I do not want to duplicate the k continuation as it may be large.
        -- This can sometimes produce useless redexes (like 1 becoming (\x -> x) 1)
        c <- newVar
        cpsthen <- cpsTail athen (AVar c)
        cpselse <- cpsTail aelse (AVar c)
        return (AApp (AFun c (AIf a cpsthen cpselse)) k)
    AEmptyCase -> return (AApp k AEmptyCase)
    AFun s ast' -> do
        c <- newVar
        res <- cpsTail ast' (AVar c)
        return (AApp k (AFun s (AFun c res)))
    ALetFun f x ast' ast2 -> do
        c <- newVar
        res <- cpsTail ast' (AVar c)
        outer <- cpsTail ast2 k
        return (AApp k (ALetFun f x (AFun c res) outer))
    ASeq ast' ast2 -> do
        ast2' <- cpsTail ast2 k
        cps ast' (\a -> return $ ASeq a ast2')

toCPS :: AST -> AST
toCPS ast = evalState (cps ast return) 0