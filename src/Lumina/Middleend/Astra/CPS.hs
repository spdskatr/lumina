module Lumina.Middleend.Astra.CPS (cps, cpsTail, toCPS) where

{-
 - Converts code to continutation passing style, eliminating all tail recursion
 - and introducing a whole load of lambdas.
 - 
 - Note that this code still produces a few eta-redexes (terms of the form 
 - "fun x -> f x end"), which I will eliminate to just "f" in the next step.
 -}
{-
 - Converts code to continutation passing style, eliminating all tail recursion
 - and introducing a whole load of lambdas.
 - 
 - Note that this code still produces a few eta-redexes (terms of the form 
 - "fun x -> f x end"), which I will eliminate to just "f" in the next step.
 -}
import Lumina.Middleend.Astra.Astra (AST(..), BinaryOp(..), astraType)

import Control.Monad.Trans.State.Strict (State, get, put, evalState)
import Lumina.Middleend.Typing (LuminaType (..), getReturnType)

type CPSTable a = State Int a

newVar :: String -> State Int String
newVar label = do
    i <- get
    put (i+1)
    return $ show i ++ label

liftCont :: String -> LuminaType -> LuminaType -> (AST -> CPSTable AST) -> CPSTable AST
liftCont label t tres k = do
    r <- newVar label
    inner <- k (AVar r t)
    return $ AFun r t inner (TFun t tres)

cps :: AST -> LuminaType -> (AST -> CPSTable AST) -> CPSTable AST
cps ast tres k = case ast of
    ABool b -> k (ABool b)
    AInt n -> k (AInt n)
    AUnit -> k AUnit
    AVar s t -> k (AVar s t)
    AApp ast' ast2 t -> do
        cont <- liftCont "cont" t tres k
        cps ast' tres (\f -> cps ast2 tres (\a -> return (AApp (AApp f a (TFun t tres)) cont tres)))
    AUnaryOp uo ast' t -> cps ast' tres (\a -> k $ AUnaryOp uo a t)
    ABinaryOp bo ast' ast2 t ->
        case bo of
            OpOr -> cps (AIf ast' (ABool True) ast2 TBool) tres k
            OpAnd -> cps (AIf ast' ast2 (ABool False) TBool) tres k
            _ -> cps ast' tres (\a -> cps ast2 tres (\b -> k $ ABinaryOp bo a b t))
    AAssign ast' ast2 -> cps ast' tres (\a -> cps ast2 tres (k . AAssign a))
    AIf acond athen aelse t -> cps acond tres $ \a -> do
        -- Note: I do not want to duplicate the k continuation as it may be large.
        -- Hence, I lift the continuation to be evaluated at execution time.
        -- Correction: The continuation seems to be replicated at most twice, since
        -- cps_tail will never replicate the continuation it's given.
        c <- liftCont "jump" t tres k
        cpsthen <- cpsTail athen tres c
        cpselse <- cpsTail aelse tres c
        return (AIf a cpsthen cpselse tres)
    AFun s t ast' t' -> do
        let rt = getReturnType t'
        let kt = TFun rt rt
        let ct = TFun kt rt
        c <- newVar "k"
        res <- cpsTail ast' tres (AVar c kt)
        k (AFun s t (AFun c kt res ct) (TFun t ct))
    ALet x t ast' ast2 _ -> do
        res <- cps ast2 tres k
        cps ast' tres (\a -> return $ AApp (AFun x t res (TFun t tres)) a tres)
    ALetFun f x t ast' ast2 t' -> do
        let rt = getReturnType t'
        let kt = TFun rt rt
        let ct = TFun kt rt
        c <- newVar "k"
        res <- cpsTail ast' tres (AVar c kt)
        outer <- cps ast2 tres k
        return (ALetFun f x t (AFun c kt res ct) outer tres)
    ASeq ast' ast2 _ -> do
        -- We need to lift the continuation to make sure that our expression
        -- actually gets evaluated
        c <- liftCont "jump" TUnit tres (\_ -> cps ast2 tres k)
        cpsTail ast' tres c

cpsTail :: AST -> LuminaType -> AST -> CPSTable AST
cpsTail ast tres k = case ast of
    ABool b -> return (AApp k (ABool b) tres)
    AInt n -> return (AApp k (AInt n) tres)
    AUnit -> return (AApp k AUnit tres)
    AVar s t -> return (AApp k (AVar s t) tres)
    AApp ast' ast2 t -> cps ast' tres (\f -> cps ast2 tres (\a -> return (AApp (AApp f a (TFun t tres)) k tres)))
    AUnaryOp uo ast' t -> cps ast' tres (\a -> return $ AApp k (AUnaryOp uo a t) tres)
    ABinaryOp bo ast' ast2 t ->
        case bo of
            OpOr -> cpsTail (AIf ast' (ABool True) ast2 TBool) tres k
            OpAnd -> cpsTail (AIf ast' ast2 (ABool False) TBool) tres k
            _ -> cps ast' tres (\a -> cps ast2 tres (\b -> return $ AApp k (ABinaryOp bo a b t) tres))
    AAssign ast' ast2 -> cps ast' tres (\a -> cps ast2 tres (\b -> return $ AApp k (AAssign a b) tres))
    AIf acond athen aelse t -> cps acond tres $ \a -> do
        -- Note: I do not want to duplicate the k continuation as it may be large.
        -- This can sometimes produce useless redexes (like 1 becoming (\x -> x) 1)
        let kt = TFun t tres
        c <- newVar "jump"
        cpsthen <- cpsTail athen tres (AVar c kt)
        cpselse <- cpsTail aelse tres (AVar c kt)
        return (AApp (AFun c kt (AIf a cpsthen cpselse tres) (TFun kt tres)) k tres)
    AFun s t ast' t' -> do
        let rt = getReturnType t'
        let kt = TFun rt rt
        let ct = TFun kt rt
        c <- newVar "k"
        res <- cpsTail ast' tres (AVar c kt)
        return (AApp k (AFun s t (AFun c kt res ct) (TFun t ct)) tres)
    ALet x t ast' ast2 _ -> do
        res <- cpsTail ast2 tres k
        cps ast' tres (\a -> return $ AApp (AFun x t res (TFun t tres)) a tres)
    ALetFun f x t ast' ast2 t' -> do
        let rt = getReturnType t'
        let kt = TFun rt rt
        let ct = TFun kt rt
        c <- newVar "k"
        res <- cpsTail ast' tres (AVar c kt)
        outer <- cpsTail ast2 tres k
        return (ALetFun f x t (AFun c kt res ct) outer tres)
    ASeq ast' ast2 _ -> do
        c <- liftCont "jump" TUnit tres (\_ -> cpsTail ast2 tres k)
        cpsTail ast' tres c

toCPS :: AST -> AST
toCPS ast = evalState (cps ast (astraType ast) return) 0