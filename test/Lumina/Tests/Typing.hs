module Lumina.Tests.Typing (testWellTyped) where

import Lumina.Middleend.Astra.Astra (AST (..), astraType, UnaryOp (..), BinaryOp (..), (>>:=))

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Lumina.Middleend.Typing (LuminaType (..))

assertEq :: AST -> LuminaType -> LuminaType -> Either String ()
assertEq ast t1 t2 = if t1 == t2 then Right () else Left $ "Type " ++ show t1 ++ " does not match expected type " ++ show t2 ++ ": " ++ show ast

doUnary :: UnaryOp -> LuminaType -> Either String LuminaType
doUnary OpNot TBool = Right TBool
doUnary OpRef t = Right (TRef t)
doUnary OpBang (TRef t) = Right t
doUnary uo t1 = Left $ "UnaryOp (" ++ show uo ++ ") cannot be applied to value of type " ++ show t1

doBinary :: BinaryOp -> LuminaType -> LuminaType -> Either String LuminaType
doBinary OpAdd TInt TInt = Right TInt
doBinary OpSub TInt TInt = Right TInt
doBinary OpMul TInt TInt = Right TInt
doBinary OpAnd TBool TBool = Right TBool
doBinary OpOr TBool TBool = Right TBool
doBinary OpLessThan TInt TInt = Right TBool
doBinary OpIntEqual TInt TInt = Right TBool
doBinary OpBoolEqual TBool TBool = Right TBool
doBinary bo t1 t2 = Left $ "BinaryOp (" ++ show bo ++ ") cannot be applied to values of types " ++ show t1 ++ " and " ++ show t2

checkTypes :: Map String LuminaType -> AST -> Either String ()
checkTypes env ast = case ast of
    ABool _ -> Right ()
    AInt _ -> Right ()
    AUnit -> Right ()
    AVar s t -> case env Map.!? s of
        Nothing -> die $ "unbound variable " ++ s
        Just t2 -> t <=> t2
    AApp ast' ast2 t3 -> checkChildren $ do
        let t1 = astraType ast'
        let t2 = astraType ast2
        case t1 of
            TFun t2' t3' -> do
                t2' <=> t2
                t3' <=> t3
            _ -> die $ "left side of application not a function type - found " ++ show t1
    AUnaryOp uo ast' t -> checkChildren $ do
        let t1 = astraType ast'
        t' <- doUnary uo t1 
        t' <=> t
    ABinaryOp bo ast' ast2 t -> checkChildren $ do
        let t1 = astraType ast'
        let t2 = astraType ast2
        t' <- doBinary bo t1 t2
        t' <=> t
    AAssign ast' ast2 -> checkChildren $ do
        let t1 = astraType ast'
        let t2 = astraType ast2
        case t1 of
            TRef t2' -> do
                t2' <=> t2
            _ -> die $ "Left side of assignment should be a reference - found " ++ show t1
    AIf ast' ast2 ast3 t -> checkChildren $ do
        let t1 = astraType ast'
        let t2 = astraType ast2
        let t3 = astraType ast3
        t1 <=> TBool
        t2 <=> t
        t3 <=> t
    AFun s t1 ast' t -> do
        let newEnv = Map.insert s t1 env
        checkTypes newEnv ast'
        let t2 = astraType ast'
        t <=> TFun t1 t2
    ALet s t1 ast' ast2 t2 -> do
        let newEnv = Map.insert s t1 env
        checkTypes env ast'
        checkTypes newEnv ast2
        let t1' = astraType ast'
        let t2' = astraType ast2
        t1 <=> t1'
        t2 <=> t2'
    ALetFun f s t1 ast' ast2 t2 -> do
        let t2' = astraType ast2
        let t3 = astraType ast'
        let fEnv = Map.insert f (TFun t1 t3) env
        let sEnv = Map.insert s t1 fEnv
        checkTypes sEnv ast'
        checkTypes fEnv ast2
        t2 <=> t2'
    ASeq _ b t -> checkChildren $ do
        let t' = astraType b    
        t' <=> t
    where
        checkChildren m = (\a -> checkTypes env a >> return a) >>:= ast >> m
        (<=>) = assertEq ast
        die x = Left $ x ++ ": " ++ show ast

testWellTyped :: AST -> Either String ()
testWellTyped = checkTypes Map.empty