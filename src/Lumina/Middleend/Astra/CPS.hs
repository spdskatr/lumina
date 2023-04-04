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
import Lumina.Middleend.Typing (LuminaType (..))

type CPSTable a = State Int a

newVar :: String -> State Int String
newVar label = do
    i <- get
    put (i+1)
    return $ show i ++ label

contType :: LuminaType -> LuminaType
contType TAny = TAny
contType TInt = TInt
contType TBool = TBool
contType TUnit = TUnit
contType (TRef t) = TRef (contType t)
contType (TFun t1 t2) = TFun (contType t1) (TFun (TFun (contType t2) TAny) TAny)

liftCont :: String -> LuminaType -> (AST -> CPSTable AST) -> CPSTable AST
liftCont label t k = do
    r <- newVar label
    inner <- k (AVar r t)
    return $ AFun r t inner

cps :: AST -> (AST -> CPSTable AST) -> CPSTable AST
cps ast k = case ast of
    ABool b -> k (ABool b)
    AInt n -> k (AInt n)
    AUnit -> k AUnit
    AVar s t -> k (AVar s (contType t))
    AApp ast' ast2 -> do
        let t = contType $ astraType ast
        cont <- liftCont "cont" t k
        cps ast' (\f -> cps ast2 (\a -> return (AApp (AApp f a) cont)))
    AUnaryOp uo ast' -> cps ast' (k . AUnaryOp uo)
    ABinaryOp bo ast' ast2 ->
        case bo of
            OpOr -> cps (AIf ast' (ABool True) ast2) k
            OpAnd -> cps (AIf ast' ast2 (ABool False)) k
            _ -> cps ast' (\a -> cps ast2 (k . ABinaryOp bo a))
    AAssign ast' ast2 -> cps ast' (\a -> cps ast2 (k . AAssign a))
    AIf acond athen aelse -> cps acond $ \a -> do
        -- Note: I do not want to duplicate the k continuation as it may be large.
        -- Hence, I lift the continuation to be evaluated at execution time.
        -- Correction: The continuation seems to be replicated at most twice, since
        -- cps_tail will never replicate the continuation it's given.
        let t = astraType athen
        c <- liftCont "jump" t k
        cpsthen <- cpsTail athen c
        cpselse <- cpsTail aelse c
        return (AIf a cpsthen cpselse)
    AFun s t ast' -> do
        let rt = contType $ astraType ast'
        let kt = TFun rt rt
        c <- newVar "k"
        res <- cpsTail ast' (AVar c kt)
        k (AFun s (contType t) (AFun c kt res))
    ALet x t ast' ast2 -> do
        res <- cps ast2 k
        cps ast' (return . AApp (AFun x (contType t) res))
    ALetFun f x t ast' ast2 -> do
        let rt = contType $ astraType ast'
        let kt = TFun rt rt
        c <- newVar "k"
        res <- cpsTail ast' (AVar c kt)
        outer <- cps ast2 k
        return (ALetFun f x (contType t) (AFun c kt res) outer)
    ASeq ast' ast2 -> do
        -- We need to lift the continuation to make sure that our expression
        -- actually gets evaluated
        c <- liftCont "jump" TAny (\_ -> cps ast2 k)
        cpsTail ast' c

cpsTail :: AST -> AST -> CPSTable AST
cpsTail ast k = case ast of
    ABool b -> return (AApp k (ABool b))
    AInt n -> return (AApp k (AInt n))
    AUnit -> return (AApp k AUnit)
    AVar s t -> return (AApp k (AVar s (contType t)))
    AApp ast' ast2 -> cps ast' (\f -> cps ast2 (\a -> return (AApp (AApp f a) k)))
    AUnaryOp uo ast' -> cps ast' (return . AApp k . AUnaryOp uo)
    ABinaryOp bo ast' ast2 ->
        case bo of
            OpOr -> cpsTail (AIf ast' (ABool True) ast2) k
            OpAnd -> cpsTail (AIf ast' ast2 (ABool False)) k
            _ -> cps ast' (\a -> cps ast2 (return . AApp k . ABinaryOp bo a))
    AAssign ast' ast2 -> cps ast' (\a -> cps ast2 (return . AApp k . AAssign a))
    AIf acond athen aelse -> cps acond $ \a -> do
        -- Note: I do not want to duplicate the k continuation as it may be large.
        -- This can sometimes produce useless redexes (like 1 becoming (\x -> x) 1)
        let kt = astraType k
        c <- newVar "jump"
        cpsthen <- cpsTail athen (AVar c kt)
        cpselse <- cpsTail aelse (AVar c kt)
        return (AApp (AFun c kt (AIf a cpsthen cpselse)) k)
    AFun s t ast' -> do
        let rt = contType $ astraType ast'
        let kt = TFun rt rt
        c <- newVar "k"
        res <- cpsTail ast' (AVar c kt)
        return (AApp k (AFun s (contType t) (AFun c kt res)))
    ALet x t ast' ast2 -> do
        res <- cpsTail ast2 k
        cps ast' (return . AApp (AFun x (contType t) res))
    ALetFun f x t ast' ast2 -> do
        let rt = contType $ astraType ast'
        let kt = TFun rt rt
        c <- newVar "k"
        res <- cpsTail ast' (AVar c kt)
        outer <- cpsTail ast2 k
        return (ALetFun f x (contType t) (AFun c kt res) outer)
    ASeq ast' ast2 -> do
        c <- liftCont "jump" TAny (\_ -> cpsTail ast2 k)
        cpsTail ast' c

toCPS :: AST -> AST
toCPS ast = evalState (cps ast return) 0