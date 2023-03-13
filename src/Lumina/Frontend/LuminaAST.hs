module Lumina.Frontend.LuminaAST (
    ASTType (..),
    TypeEnv,
    BinaryOp (..),
    UnaryOp (..),
    AST (..),
    toAST
) where

import Lumina.Frontend.LuminaGrammar (PAST (..))
import Data.Map (Map)
import Lumina.Utils (internalError)
import Lumina.Frontend.Lexer (Token(..))
import Control.Monad (liftM2)

import qualified Data.Map as Map

data ASTType
    = TInt
    | TBool
    | TUnit
    | TRef ASTType
    | TFun ASTType ASTType
    deriving (Eq, Show)

type TypeEnv = Map String ASTType

data BinaryOp
    = OpAdd
    | OpSub
    | OpMul
    | OpAnd
    | OpOr
    | OpLessThan
    | OpIntEqual
    | OpBoolEqual
    deriving (Eq, Show)

data UnaryOp
    = OpNot
    | OpBang
    | OpRef
    deriving (Eq, Show)

data AST
    = ABool Bool
    | AInt Int
    | AUnit
    | AVar String
    | AApp AST AST
    | AUnaryOp UnaryOp AST
    | ABinaryOp BinaryOp AST AST
    | AAssign AST AST
    | AEmptyCase AST
    | AIf AST AST AST
    | AFun String AST
    | ALetFun String String AST AST
    | AWhile AST AST
    | ASeq AST AST
    deriving (Eq)

type TranslationRes a = Either String a

instance Show AST where
    show (ABool b) = show b
    show (AInt i) = show i
    show AUnit = show "()"
    show (AVar s) = s
    show (AApp a b) = "(" ++ show a ++ " " ++ show b ++ ")"
    show (AUnaryOp uo a) = show uo ++ "(" ++ show a ++ ")"
    show (ABinaryOp bo a b) = "(" ++ show a ++ " " ++ show bo ++ " " ++ show b ++ ")"
    show (AAssign a b) = show a ++ " := " ++ show b
    show (AEmptyCase a) = "EmptyCase"
    show (AIf a b c) = "if " ++ show a ++ " then " ++ show b ++ " else " ++ show c ++  " end"
    show (AFun s b) = "fun " ++ show s ++ ": " ++ show b ++ " end"
    show (ALetFun f x a b) = "let fun " ++ f ++ " " ++ x ++ " = " ++ show a ++ " in " ++ show b ++ " end"
    show (AWhile c l) = "while " ++ show c ++ " do " ++ show l ++ " end"
    show (ASeq a b) = show a ++ "; " ++ show b

typeError :: String -> TranslationRes a
typeError s = Left $ "Type error: " ++ s

applyType :: ASTType -> ASTType -> TranslationRes ASTType
applyType t1 t2 = case t1 of
    TFun t2' t3 | t2 == t2' -> return t3
    _ -> typeError ("Unmatched types " ++ show t1 ++ " and " ++ show t2)

checkInts :: ASTType -> ASTType -> TranslationRes ASTType
checkInts t1 t2 = case (t1, t2) of
    (TInt, TInt) -> return TInt
    _ -> typeError ("Arguments passed to arithmetic operation must be of integer type; found " ++ show (t1, t2) ++ " instead")

checkBools :: ASTType -> ASTType -> TranslationRes ASTType
checkBools t1 t2 = case (t1, t2) of
    (TBool, TBool) -> return TBool
    _ -> typeError ("Arguments passed to boolean operation must be of boolean type; found " ++ show (t1, t2) ++ " instead")

checkComparison :: ASTType -> ASTType -> TranslationRes ASTType
checkComparison t1 t2 = case (t1, t2) of
    (TInt, TInt) -> return TBool
    _ -> typeError ("Arguments passed to comparison must be of integer type; found " ++ show (t1, t2) ++ " instead")

checkAssign :: ASTType -> ASTType -> TranslationRes ASTType
checkAssign t1 t2 = case t1 of
    (TRef t2') | t2' == t2 -> return TUnit
    _ -> typeError ("Expecting " ++ show (TRef t2) ++ " in assignment; found " ++ show t1 ++ " instead")

getType :: PAST -> TranslationRes ASTType
getType PTInt = return TInt
getType PTBool = return TBool
getType PTUnit = return TUnit
getType (PTRef x) = TRef <$> getType x
getType (PTFun x y) = liftM2 TFun (getType x) (getType y)
getType x = typeError ("Expression is not a type: " ++ show x)

getEqualsOp :: ASTType -> ASTType -> Maybe BinaryOp
getEqualsOp TInt TInt = Just OpIntEqual
getEqualsOp TBool TBool = Just OpBoolEqual
getEqualsOp _ _ = Nothing

makeCases :: (String, ASTType) -> TypeEnv -> [(PAST, PAST)] -> TranslationRes (AST, ASTType)
makeCases _ _ [] = typeError "No matches in case expression"
makeCases (s,t) env [(pa,pb)] =
    case pa of
        PVar (PToken (Ident x)) | Map.notMember x env -> do
            let newEnv = Map.insert x t env
            (a2,t2) <- translate newEnv pb
            return (AApp (AFun x a2) (AVar s), t2)
        _ -> do
            (a1,t1) <- translate env pa
            (a2,t2) <- translate env pb
            case getEqualsOp t t1 of
                Just op -> return (AIf (ABinaryOp op (AVar s) a1) a2 (AEmptyCase (AVar s)), t2)
                Nothing -> typeError ("Case split expecting equatable types, got types " ++ show (t, t1) ++ " instead")
makeCases (s,t) env ((pa,pb):rest) = do
    (a1,t1) <- translate env pa
    (a2,t2) <- translate env pb
    (aa,t') <- makeCases (s,t) env rest
    case getEqualsOp t t1 of
        Nothing -> typeError ("Case split expecting equatable types, got types " ++ show (t, t1) ++ " instead")
        Just op -> if t2 /= t' then 
            typeError ("Result types of case split must be equal, got types " ++ show (t2, t') ++ " instead")
        else 
            return (AIf (ABinaryOp op (AVar s) a1) a2 aa, t2)

translate :: TypeEnv -> PAST -> TranslationRes (AST, ASTType)
translate env past = case past of
    PToken _ -> internalError "Found lonely token"
    PZero -> return (AInt 0, TInt)
    PTrue -> return (ABool True, TBool)
    PFalse -> return (ABool False, TBool)
    PUnit -> return (AUnit, TUnit)
    PVar (PToken (Ident x)) -> do
        case Map.lookup x env of
            Nothing -> typeError ("Unbound variable " ++ x)
            Just t -> return (AVar x, t)
    PInt (PToken (IntLit x)) -> return (AInt x, TInt)
    PApp pa pa' -> do
        (a1,t1) <- translate env pa
        (a2,t2) <- translate env pa'
        t <- applyType t1 t2
        return (AApp a1 a2, t)
    PBang pa -> do
        (a1, t1) <- translate env pa
        case t1 of
            TRef x -> return (AUnaryOp OpBang a1, x)
            TBool -> return (AUnaryOp OpNot a1, TBool)
            _ -> typeError ("Mismatched type for ! (should be bool or reference): " ++ show t1)
    PRef pa -> do
        (a1, t1) <- translate env pa
        return (AUnaryOp OpRef a1, TRef t1)
    PMul pa pa' -> do
        (a1,t1) <- translate env pa
        (a2,t2) <- translate env pa'
        t <- checkInts t1 t2
        return (ABinaryOp OpMul a1 a2, t)
    PSub pa pa' -> do
        (a1,t1) <- translate env pa
        (a2,t2) <- translate env pa'
        t <- checkInts t1 t2
        return (ABinaryOp OpSub a1 a2, t)
    PAdd pa pa' -> do
        (a1,t1) <- translate env pa
        (a2,t2) <- translate env pa'
        t <- checkInts t1 t2
        return (ABinaryOp OpAdd a1 a2, t)
    PAnd pa pa' -> do
        (a1,t1) <- translate env pa
        (a2,t2) <- translate env pa'
        t <- checkBools t1 t2
        return (ABinaryOp OpAnd a1 a2, t)
    POr pa pa' -> do
        (a1,t1) <- translate env pa
        (a2,t2) <- translate env pa'
        t <- checkBools t1 t2
        return (ABinaryOp OpOr a1 a2, t)
    PLessThan pa pa' -> do
        (a1,t1) <- translate env pa
        (a2,t2) <- translate env pa'
        t <- checkComparison t1 t2
        return (ABinaryOp OpLessThan a1 a2, t)
    PEqual pa pa' -> do
        (a1,t1) <- translate env pa
        (a2,t2) <- translate env pa'
        let eqOp = getEqualsOp t1 t2
        case eqOp of 
            Just x -> return (ABinaryOp x a1 a2, TBool)
            Nothing -> typeError ("Expected equatable types for = comparison; got " ++ show (t1, t2) ++ " instead")
    PAssign pa pa' -> do
        (a1,t1) <- translate env pa
        (a2,t2) <- translate env pa'
        t <- checkAssign t1 t2
        return (AAssign a1 a2, t)
    PWhile pa pa' -> do
        (a1,t1) <- translate env pa
        (a2,_) <- translate env pa'
        if t1 == TBool then 
            return (AWhile a1 a2, TUnit)
        else 
            typeError ("Expected boolean type for expression to while; got " ++ show t1 ++ " instead")
    PSeq pa pa' -> do
        (a1,_) <- translate env pa
        (a2,t2) <- translate env pa'
        return (ASeq a1 a2, t2)
    PFun (PToken (Ident x)) ta ta' pa' -> do
        inType <- getType ta
        let newEnv = Map.insert x inType env
        (a1,t1) <- translate newEnv pa'
        t2 <- getType ta'
        if t1 == t2 then 
            return (AFun x a1, TFun inType t2)
        else
            typeError ("Expected type " ++ show t2 ++ " in lambda, got " ++ show t1 ++ " instead")
    PLet (PToken (Ident x)) ta pa pa' -> do
        actualType <- getType ta
        let newEnv = Map.insert x actualType env
        (a1,t1) <- translate env pa
        (a2,t2) <- translate newEnv pa'
        if t1 == actualType then
            return (AApp (AFun x a2) a1, t2)
        else
            typeError ("Expected type " ++ show actualType ++ " in with, got " ++ show t1 ++ " instead")
    
    -- with fun f (x : A) -> B = a' do b' end
    -- with f : (A -> B) = fun (x : A) : B -> a' end do b' end
    PLetFun (PToken (Ident f)) (PToken (Ident x)) ta ta' pa' pb -> do
        inType <- getType ta
        outType <- getType ta'
        let newEnvB = Map.insert f (TFun inType outType) env
        let newEnvA = Map.insert x inType newEnvB
        (a1,t1) <- translate newEnvA pa'
        (a2,t2) <- translate newEnvB pb
        if t1 == outType then
            return (ALetFun f x a1 a2, t2)
        else
            typeError ("Expected type " ++ show outType ++ " in body of function " ++ f ++ ", got " ++ show t1 ++ " instead")
    PCase pa (PCaseList l) -> do
        (a1,t1) <- translate env pa
        (a2,t2) <- makeCases ("0case",t1) env l
        return (AApp (AFun "0case" a2) a1, t2)
    PCaseList _ -> internalError "Found lonely CaseList"
    _ -> internalError $ "Bad PAST: " ++ show past

toAST :: PAST -> (AST, ASTType)
toAST past = case translate Map.empty past of
  Left s -> error s
  Right a -> a