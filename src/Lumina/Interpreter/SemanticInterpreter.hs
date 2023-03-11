module Lumina.Interpreter.SemanticInterpreter (
    Store,
    Env,
    Value(..),
    interpret,
    getValue,
    eval
) where

import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Data.Ix (Ix)
import Data.Map (Map)
import qualified Data.Map as Map
import Lumina.Frontend.LuminaAST (AST (..), UnaryOp (..), BinaryOp (..), ASTType)
import Lumina.Utils (internalError, interpError)
import Lumina.Frontend.ParserGen (LRParser)
import Lumina.Frontend.Lexer (TokenTag)
import Lumina.Frontend.LuminaGrammar (LNT)
import Lumina.Frontend.Shortcuts (getAST)

newtype StoreAddress = StoreAddress Int deriving (Show, Eq, Ord, Ix)

type Store = Map StoreAddress Value
type Env = Map String Value

data Value
    = VInt Int
    | VBool Bool
    | VUnit
    | VRef StoreAddress
    | VFun (Value -> State Store Value)

instance Show Value where
    show (VInt i) = show i
    show (VBool b) = show b
    show VUnit = show "unit"
    show (VRef (StoreAddress a)) = "ref @ " ++ show a
    show (VFun _) = "(fun)"
    
(===) :: Value -> Value -> Bool
VInt x === VInt y = x == y
VBool a === VBool b = a == b
VUnit === VUnit = True
_ === _ = False

applyUnaryOp :: UnaryOp -> Value -> State Store Value
applyUnaryOp OpNot (VBool b) = return $ VBool (not b)
applyUnaryOp OpBang (VRef r) = do
    store <- get
    return $ store Map.! r
applyUnaryOp OpRef v = do
    store <- get
    let i = StoreAddress $ Map.size store
    put $ Map.insert i v store
    return (VRef i)
applyUnaryOp uo val = internalError ("Bad UnaryOp: " ++ show uo ++ " on " ++ show val)

applyBinaryOp :: BinaryOp -> Value -> Value -> State Store Value
applyBinaryOp OpAdd (VInt i) (VInt j) = return $ VInt (i+j)
applyBinaryOp OpSub (VInt i) (VInt j) = return $ VInt (i-j)
applyBinaryOp OpMul (VInt i) (VInt j) = return $ VInt (i*j)
applyBinaryOp OpAnd (VBool a) (VBool b) = return $ VBool (a && b)
applyBinaryOp OpOr (VBool a) (VBool b) = return $ VBool (a || b)
applyBinaryOp OpLessThan (VInt i) (VInt j) = return $ VBool (i < j)
applyBinaryOp OpIntEqual (VInt i) (VInt j) = return $ VBool (i == j)
applyBinaryOp OpBoolEqual (VBool a) (VBool b) = return $ VBool (a == b)
applyBinaryOp bo v1 v2 = internalError ("Bad BinaryOp " ++ show bo ++ " on " ++ show v1 ++ " and " ++ show v2)

foldCase :: Env -> Value -> [(AST, AST)] -> State Store Value
foldCase _ v [] = interpError $ "No case match found for expression " ++ show v
foldCase env v ((AVar x, ast):rest) = case Map.lookup x env of
    Nothing -> do
        let newEnv = Map.insert x v env
        interpret ast newEnv
    Just val -> do
        if val === v then
            interpret ast env
        else
            foldCase env v rest
foldCase env v ((astm, ast):rest) = do
    val <- interpret astm env
    if val === v then
        interpret ast env
    else
        foldCase env v rest

interpret :: AST -> Env -> State Store Value
interpret a env = case a of
    ABool b -> return (VBool b)
    AInt n -> return (VInt n)
    AUnit -> return VUnit
    AVar s -> return $ env Map.! s
    AApp a1 a2 -> do
        val1 <- interpret a1 env
        case val1 of
            VFun f -> do
                val2 <- interpret a2 env
                f val2
            _ -> internalError $ "Left argument of function application should be a function, found " ++ show val1 ++ " instead"
    AUnaryOp uo ast' -> do
        val <- interpret ast' env
        applyUnaryOp uo val
    ABinaryOp bo ast ast' -> do
        val1 <- interpret ast env
        -- Short circuiting
        case (bo, val1) of
            (OpAnd, VBool False) -> return (VBool False)
            (OpOr, VBool True) -> return (VBool True)
            _ -> do
                val2 <- interpret ast' env
                applyBinaryOp bo val1 val2
    AAssign ast ast' -> do
        val1 <- interpret ast env
        val2 <- interpret ast' env
        case val1 of
            VRef loc -> do
                store <- get
                put $ Map.insert loc val2 store
                return VUnit
            _ -> internalError $ "Left side of assignment-expression did not evaluate to a reference, found " ++ show val1 ++ " instead"
    ACase ast x0 -> do
        val1 <- interpret ast env
        foldCase env val1 x0
    AFun s ast' -> 
        return $ VFun $ \val -> do
            let newEnv = Map.insert s val env
            interpret ast' newEnv
    ALetFun f x ast ast' -> do
        let envF = Map.insert f (VFun $ \val -> interpret ast (Map.insert x val envF)) env
        interpret ast' envF
    AWhile ast ast' -> do
        val1 <- interpret ast env
        if val1 === VBool True then
            interpret (ASeq [ast', AWhile ast ast']) env
        else
            return VUnit
    ASeq asts -> case asts of
      [] -> internalError "Empty ASeq - this should literally be impossible"
      ast : rest -> do
        res <- interpret ast env
        case rest of
            [] -> return res
            _ -> interpret (ASeq rest) env
--  _ -> internalError $ "Bad AST (which should never happen): " ++ show a

getValue :: AST -> Value
getValue ast = evalState (interpret ast Map.empty) Map.empty

eval :: LRParser LNT TokenTag -> String -> (Value, ASTType)
eval lr code = 
    let (ast, astt) = getAST lr code
    in (getValue ast, astt)