{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module LuminaCodeTest (runCodeTest) where

import Lumina.Frontend.Shortcuts (loadParserFrom, getAST)
import Lumina.Interpreter.SemanticInterpreter (Value (..), eval, getValue)
import Lumina.Middleend.Shortcuts (transform)
import Lumina.Frontend.ParserGen (LRParser)
import Lumina.Frontend.Lexer (TokenTag)
import Lumina.Frontend.LuminaGrammar (LNT)
import Lumina.Frontend.LuminaAST (AST (..), freeVars, (><>))

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (forM_)

type TestCase = (String, String, Value)

testCases :: [TestCase]
testCases = [
    ("arith"
    ,"2 + 3 * (5 - 6)"
    ,VInt (-1)),
    ("lambda"
    ,"fun (x : int) : int -> x + 3 end 3",
    VInt 6),
    ("withFun"
    ,"with fun f(x:int) : int = 5 do f 4 end"
    ,VInt 5),
    ("sideeffect"
    ,"with x : int# = #5 do x := 2; !x end"
    ,VInt 2),
    ("casematch"
    ,"with 5 case | x -> x+2 end"
    ,VInt 7),
    ("fib"
    ,"with fun fib (x: int) : int = with x case | 0 -> 0 | 1 -> 1 | _ -> fib (x - 1) + fib (x - 2) end do fib 6 end"
    ,VInt 8),
    ("ref"
    ,"with a : int # = # 0 do a := 1; !a + 1 end"
    ,VInt 2),
    ("countup"
    ,"with a : int# = #0 do while !a < 5 do a := !a + 1 end; !a end"
    ,VInt 5),
    ("applicationOrder"
    ,"with a : int# = #0 do fun (x : int) : int -> (a := 1; fun (y : int) : int -> y end) x end (a := !a * 2; 2); !a end"
    ,VInt 1),
    ("iterfib",
    "with a : int # = # 0 do with b : int # = # 1 do with c : int # = # 2 do with n : int # = # 5 do while !(!n = 0) do n := !n - 1; c := !a; a := !b; b := !b + !c; end end end; !b end end"
    ,VInt 8),
    ("higherOrder"
    ,"with f : (int -> int -> int) = fun (x : int) : (int -> int) -> fun (y : int) : int -> x + y end end do f 3 5 end"
    ,VInt 8),
    ("91"
    ,"with fun m (n : int) : int = with n < 101 case | true -> m (m (n + 11)) | false -> n - 10 end do with a : int # = # 0 do while m !a = 91 do a := !a + 1 end; !a end end"
    ,VInt 102),
    ("sqrt"
    ,"with fun sqrt (x : int) : int = with fun sqrtHelper (x : int) : (int -> int) = fun (y : int) : int -> with z : int = 2 * y - 1 do with z < x+1 case | true -> sqrtHelper (x-z) (y+1) | false -> y-1 end end end do sqrtHelper x 1 end do sqrt 60 end"
    ,VInt 7)]

equalValues :: Value -> Value -> Bool
equalValues VUnit VUnit = True
equalValues (VBool a) (VBool b) = a == b
equalValues (VInt a) (VInt b) = a == b
equalValues _ _ = False

checkFreeVarsAgreeWithEnv :: String -> Set String -> AST -> [String]
checkFreeVarsAgreeWithEnv name env ast =
    case filter (`Set.notMember` env) (freeVars ast) of
        [] -> case ast of
            AFun x a1 -> checkFreeVarsAgreeWithEnv name (Set.insert x env) a1
            ALetFun f x a1 a2 ->
                let envF = Set.insert f env
                    envX = Set.insert x envF
                in checkFreeVarsAgreeWithEnv name envX a1 <> checkFreeVarsAgreeWithEnv name envF a2
            _ -> checkFreeVarsAgreeWithEnv name env ><> ast
        l -> ["test " ++ name ++ ": AST " ++ show ast ++ " failed free variables check; variable " ++ x ++ " not found\n" | x <- l]

testFreeVars :: (String -> AST) -> TestCase -> Either String ()
testFreeVars e (name, code, _) =
    case checkFreeVarsAgreeWithEnv name Set.empty (e code) of
        [] -> Right ()
        l -> Left $ "(" ++ show (length l) ++ " error(s) total) " ++ head l

regressionTest :: (String -> Value) -> TestCase -> Either String ()
regressionTest e (name, code, val) =
    let res = e code in
    if equalValues res val then Right () else Left $ "test " ++ name ++ " failed; expected " ++ show val ++ ", got " ++ show res

testWithEvaluator :: (String -> Value) -> Either String ()
testWithEvaluator e = do
    forM_ testCases (regressionTest e)

allTests :: LRParser LNT TokenTag -> Either String ()
allTests lr = do
    testWithEvaluator (fst . eval lr)
    testWithEvaluator (getValue . transform . fst . getAST lr)
    forM_ testCases $ testFreeVars (fst . getAST lr)

runCodeTest :: IO ()
runCodeTest = do
    lr <- loadParserFrom "data/lr1.txt"
    case allTests lr of
        Left err -> error err
        Right _ -> putStrLn "LuminaCodeTest PASS"
