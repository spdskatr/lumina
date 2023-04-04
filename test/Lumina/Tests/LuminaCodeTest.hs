{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Lumina.Tests.LuminaCodeTest (runCodeTest) where

import Lumina.Frontend.Shortcuts (loadParserFrom, getAST)
import Lumina.Interpreter.AstraInterpreter (Value (..), eval, getValue, getValueCF)
import Lumina.Middleend.Shortcuts (transform)
import Lumina.Frontend.ParserGen (LRParser)
import Lumina.Frontend.Lexer (TokenTag)
import Lumina.Frontend.LuminaGrammar (LNT)
import Lumina.Middleend.Astra.Astra (AST (..))

import Control.Monad (forM_)
import Lumina.Tests.Typing (testWellTyped)

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
    ("higherOrderSimple"
    ,"fun (x : int) : (int -> int) -> fun (y : int) : int -> x + y end end 3 5"
    ,VInt 8),
    ("higherOrder"
    ,"with f : (int -> int -> int) = fun (x : int) : (int -> int) -> fun (y : int) : int -> x + y end end do f 3 5 end"
    ,VInt 8),
    ("91"
    ,"with fun m (n : int) : int = with n < 101 case | true -> m (m (n + 11)) | _ -> n - 10 end do with a : int # = # 0 do while m !a = 91 do a := !a + 1 end; !a end end"
    ,VInt 102),
    ("sqrt"
    ,"with fun sqrt (x : int) : int = with fun sqrtHelper (x : int) : (int -> int) = fun (y : int) : int -> with z : int = 2 * y - 1 do with z < x+1 case | true -> sqrtHelper (x-z) (y+1) | _ -> y-1 end end end do sqrtHelper x 1 end do sqrt 60 end"
    ,VInt 7),
    ("shadowingCase"
    ,"with 5 case | x -> (fun (x:int):int -> x + 1 end) 1 end"
    ,VInt 2),
    ("shadowing"
    ,"with a : int = 1 do with fun f (x : int) : int = with b : int = a do with a : int = 2 do with x case | 0 -> b | _ -> f (x-1) end end end do f 5 end end"
    ,VInt 1),
    ("shortCircuit"
    ,"with a : int# = #0 do true || (a := !a + 1; true); false && (a := !a + 1; false); !a end"
    ,VInt 0),
    ("fibCPS"
    ,"with fun fibcps (x : int) : ((int -> int) -> int) = fun (k : (int -> int)) : int -> with x case | 0 -> k 1 | 1 -> k 1 | _ -> fibcps (x-1) (fun (r : int) : int -> fibcps (x-2) (fun (s : int) : int -> k (r+s) end) end) end end do fibcps 5 (fun (x:int) : int -> x end) end"
    ,VInt 8)
    ]

equalValues :: Value -> Value -> Bool
equalValues VUnit VUnit = True
equalValues (VBool a) (VBool b) = a == b
equalValues (VInt a) (VInt b) = a == b
equalValues _ _ = False

testTyping :: String -> (String -> AST) -> TestCase -> Either String ()
testTyping codename e (name, code, _) = 
    let a = e code
    in case testWellTyped a of
      Left err -> Left $ "test testTyping_" ++ codename ++ "_" ++ name ++ " failed: " ++ err
      Right () -> Right ()

regressionTest :: String -> (String -> Value) -> TestCase -> Either String ()
regressionTest codename e (name, code, val) =
    let res = e code in
    if equalValues res val then Right () else Left $ "test " ++ codename ++ "_" ++ name ++ " failed; expected " ++ show val ++ ", got " ++ show res

testWithEvaluator :: String -> (String -> Value) -> Either String ()
testWithEvaluator codename e =
    forM_ testCases (regressionTest codename e)

allTests :: LRParser LNT TokenTag -> Either String ()
allTests lr = do
    forM_ testCases $ testTyping "ast" (fst . getAST lr)
    testWithEvaluator "interp" (fst . eval lr)
    forM_ testCases $ testTyping "cps" (transform . fst . getAST lr)
    testWithEvaluator "interpCPS" (getValue . transform . fst . getAST lr)
    testWithEvaluator "interpContForm" (getValueCF . fst . getAST lr)

runCodeTest :: IO ()
runCodeTest = do
    lr <- loadParserFrom "data/lr1.txt"
    case allTests lr of
        Left err -> error err
        Right _ -> putStrLn "Lumina.Tests.LuminaCodeTest PASS"
