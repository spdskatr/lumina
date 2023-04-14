{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Lumina.Tests.LuminaCodeTest (runCodeTest) where

import Lumina.Frontend.Shortcuts (loadParserFrom, getAST)
import Lumina.Interpreter.AstraInterpreter (Value (..), eval)
import Lumina.Frontend.ParserGen (LRParser)
import Lumina.Frontend.Lexer (TokenTag)
import Lumina.Frontend.LuminaGrammar (LNT)
import Lumina.Middleend.Astra.Astra (AST (..))
import Lumina.Tests.Typing (testWellTyped)
import Lumina.Interpreter.MonaInterpreter (getMonaValue)
import Lumina.Middleend.Mona.Mona (astraToMona)
import Lumina.Middleend.Shortcuts (optMonaProgram, astraToCelia)
import Lumina.Backend.CeliaToC (celiaToC)

import Control.Monad (forM_, when)
import Text.Read (readMaybe)
import System.Process.Extra (readProcess, system)
import System.Exit (ExitCode(..))

type TestCase = (String, String, Int)

testCases :: [TestCase]
testCases = [
    ("arith"
    ,"2 + 3 * (5 - 6)"
    ,(-1)),
    ("lambda"
    ,"fun (x : int) : int -> x + 3 end 3",
    6),
    ("withFun"
    ,"with fun f(x:int) : int = 5 do f 4 end"
    ,5),
    ("sideeffect"
    ,"with x : int# = #5 do x := 2; !x end"
    ,2),
    ("casematch"
    ,"with 5 case | x -> x+2 end"
    ,7),
    ("fib"
    ,"with fun fib (x: int) : int = with x case | 0 -> 0 | 1 -> 1 | _ -> fib (x - 1) + fib (x - 2) end do fib 6 end"
    ,8),
    ("ref"
    ,"with a : int # = # 0 do a := 1; !a + 1 end"
    ,2),
    ("countup"
    ,"with a : int# = #0 do while !a < 5 do a := !a + 1 end; !a end"
    ,5),
    ("applicationOrder"
    ,"with a : int# = #0 do fun (x : int) : int -> (a := 1; fun (y : int) : int -> y end) x end (a := !a * 2; 2); !a end"
    ,1),
    ("iterfib",
    "with a : int # = # 0 do with b : int # = # 1 do with c : int # = # 2 do with n : int # = # 5 do while !(!n = 0) do n := !n - 1; c := !a; a := !b; b := !b + !c; end end end; !b end end"
    ,8),
    ("higherOrderSimple"
    ,"fun (x : int) : (int -> int) -> fun (y : int) : int -> x + y end end 3 5"
    ,8),
    ("higherOrder"
    ,"with f : (int -> int -> int) = fun (x : int) : (int -> int) -> fun (y : int) : int -> x + y end end do f 3 5 end"
    ,8),
    ("91"
    ,"with fun m (n : int) : int = with n < 101 case | true -> m (m (n + 11)) | _ -> n - 10 end do with a : int # = # 0 do while m !a = 91 do a := !a + 1 end; !a end end"
    ,102),
    ("sqrt"
    ,"with fun sqrt (x : int) : int = with fun sqrtHelper (x : int) : (int -> int) = fun (y : int) : int -> with z : int = 2 * y - 1 do with z < x+1 case | true -> sqrtHelper (x-z) (y+1) | _ -> y-1 end end end do sqrtHelper x 1 end do sqrt 60 end"
    ,7),
    ("shadowingCase"
    ,"with 5 case | x -> (fun (x:int):int -> x + 1 end) 1 end"
    ,2),
    ("shadowing"
    ,"with a : int = 1 do with fun f (x : int) : int = with b : int = a do with a : int = 2 do with x case | 0 -> b | _ -> f (x-1) end end end do f 5 end end"
    ,1),
    ("shortCircuit"
    ,"with a : int# = #0 do true || (a := !a + 1; true); false && (a := !a + 1; false); !a end"
    ,0),
    ("fibCPS"
    ,"with fun fibcps (x : int) : ((int -> int) -> int) = fun (k : (int -> int)) : int -> with x case | 0 -> k 1 | 1 -> k 1 | _ -> fibcps (x-1) (fun (r : int) : int -> fibcps (x-2) (fun (s : int) : int -> k (r+s) end) end) end end do fibcps 5 (fun (x:int) : int -> x end) end"
    ,8)
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
    if equalValues res (VInt val) then Right () else Left $ "test " ++ codename ++ "_" ++ name ++ " failed; expected " ++ show val ++ ", got " ++ show res

testCProgram :: String -> (String -> AST) -> TestCase -> IO ()
testCProgram codename e (name, code, val) = do
    let ccode = celiaToC $ astraToCelia $ e code
    writeFile "runtime/program.c" ccode
    putStrLn ("Running test " ++ codename ++ "_" ++ name)
    ensureSystem "clang -I runtime runtime/program.c runtime/main.c runtime/runtime.c -fsanitize=address -fsanitize=undefined -o runtime/program"
    res <- readProcess "./runtime/program" [] ""
    when (readMaybe res /= Just val) $ fail ("test " ++ codename ++ "_" ++ name ++ " failed; Expected " ++ show val ++ ", got " ++ res)
    where
        ensureSystem cmd = do
            ex <- system cmd
            when (ex /= ExitSuccess) $ fail ("Failed! Exit code: " ++ show ex)

testWithEvaluator :: String -> (String -> Value) -> Either String ()
testWithEvaluator codename e =
    forM_ testCases (regressionTest codename e)

allTests :: LRParser LNT TokenTag -> Either String ()
allTests lr = do
    forM_ testCases $ testTyping "ast" (fst . getAST lr)
    testWithEvaluator "interp" (fst . eval lr)
    testWithEvaluator "interpMona" (getMonaValue . astraToMona . fst . getAST lr)
    testWithEvaluator "interpMonaTwo" (getMonaValue . optMonaProgram . astraToMona . fst . getAST lr)

runCodeTest :: IO ()
runCodeTest = do
    lr <- loadParserFrom "data/lr1.txt"
    case allTests lr of
        Left err -> error err
        Right _ -> return ()
    forM_ testCases $ testCProgram "celiaToCBackend" (fst . getAST lr) 
    putStrLn "Lumina.Tests.LuminaCodeTest PASS"
