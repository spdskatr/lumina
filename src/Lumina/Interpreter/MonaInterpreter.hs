module Lumina.Interpreter.MonaInterpreter (getMonaValue) where
import Lumina.Middleend.Mona.Mona (MExpr (..), MAtom (..), MonaTranslationUnit, MonaFunction (..), MOper (..))

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Lumina.Utils (internalError, orElse)
import Lumina.Interpreter.AstraInterpreter (Value (..), Env, Store, applyUnaryOp, applyBinaryOp)
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Lumina.Middleend.Astra.HoistFunctions (TypedVar(TypedVar))

type ClosureEnv = Map String ([Value] -> Value -> State Store Value)

getAtomValue :: Env -> MAtom -> Value
getAtomValue _ MUnit = VUnit
getAtomValue _ (MInt i) = VInt i
getAtomValue _ (MBool b) = VBool b
getAtomValue e (MVar x) = e Map.!? x `orElse` internalError ("Variable " ++ show x ++ " not found")

getOperValue :: ClosureEnv -> Env -> MOper -> State Store Value
getOperValue _ env (MJust a) = return $ getAtomValue env a
getOperValue _ env (MApp a b) = case getAtomValue env a of
    VFun f -> f (getAtomValue env b)
    v -> internalError ("Left side of application is not a function, found " ++ show v ++ " instead")
getOperValue _ env (MUnary uo a) = applyUnaryOp uo (getAtomValue env a)
getOperValue _ env (MBinary bo a b) = applyBinaryOp bo (getAtomValue env a) (getAtomValue env b)
getOperValue cenv env (MMkClosure c as) = do
    let cl = cenv Map.!? c `orElse` internalError ("Could not make closure: function " ++ show c ++ " not found")
    return $ VFun $ cl [getAtomValue env a | a <- as]

interpretMona :: ClosureEnv -> Env -> MExpr -> State Store Value
interpretMona cenv env (MLet x _ o rest) = do
    v <- getOperValue cenv env o
    interpretMona cenv (Map.insert x v env) rest
interpretMona cenv env (MAssign x a rest) = do
    let v1 = getAtomValue env x
        v2 = getAtomValue env a
    case v1 of
        VRef ad -> do
            store <- get
            put $ Map.insert ad v2 store
        _ -> internalError $ "Left side of assignment-expression did not evaluate to a reference, found " ++ show v1 ++ " instead"
    interpretMona cenv env rest
interpretMona _ env (MReturn a) = return (getAtomValue env a)
interpretMona cenv env (MIf a th el) = do
    let v = getAtomValue env a
    case v of
        VBool True -> interpretMona cenv env th
        VBool False -> interpretMona cenv env el
        _ -> internalError $ "If statement wants boolean as condition, found " ++ show v ++ " instead"

makeEnv :: MonaTranslationUnit -> ClosureEnv
makeEnv fs = Map.map (\(MonaFunction _ fv x _ _ e) args v -> 
    let boundMap = Map.fromList $ zipWith (\(TypedVar k _) arg -> (k, arg)) fv args
    in interpretMona (makeEnv fs) (Map.insert x v boundMap) e) fs

getMonaValue :: MonaTranslationUnit -> Value
getMonaValue ctx = case ctx Map.!? "0main" of
    Just m ->
        let cenv = makeEnv ctx
            st = interpretMona cenv (Map.singleton (getArg m) VUnit) (getBody m)
        in evalState st Map.empty
    Nothing -> internalError "No main function found in translation unit; could not start interpreter"