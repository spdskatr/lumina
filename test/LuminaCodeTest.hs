{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module LuminaCodeTest (runCodeTest) where
import Lumina.Frontend.Shortcuts (loadParserFrom, getAST)
import Lumina.Interpreter.SemanticInterpreter (Value (..), eval, getValue)
import Lumina.Middleend.CPSConvert (toCPS)

type TestCase = (String -> Value) -> Either String ()

test_arith :: TestCase
test_arith e = case e "2 + 3 * (5 - 6)" of
    VInt (-1) -> Right ()
    r -> Left $ "test_arith FAIL, got " ++ show r

test_lambda :: TestCase
test_lambda e = case e "fun (x : int) : int -> x + 3 end 3" of
    VInt 6 -> Right ()
    r -> Left $ "test_lambda FAIL, got " ++ show r

test_withfun :: TestCase
test_withfun e = case e "with fun f(x:int) : int = 5 do f 4 end" of
    VInt 5 -> Right ()
    r -> Left $ "test_withfun FAIL, got " ++ show r

test_casematch :: TestCase
test_casematch e = case e "with 5 case | x -> x+2 end" of
    VInt 7 -> Right () 
    r -> Left $ "test_casematch FAIL, got " ++ show r

test_fib :: TestCase
test_fib e = case e "with fun fib (x: int) : int = with x case | 0 -> 0 | 1 -> 1 | _ -> fib (x - 1) + fib (x - 2) end do fib 6 end" of
    VInt 8 -> Right ()
    r -> Left $ "test_fib FAIL, got " ++ show r

test_ref :: TestCase
test_ref e = case e "with a : int # = # 0 do a := 1; !a + 1 end" of
    VInt 2 -> Right ()
    r -> Left $ "test_ref FAIL, got " ++ show r

test_iterfib :: TestCase
test_iterfib e = case e "with a : int # = # 0 do with b : int # = # 1 do with c : int # = # 2 do with n : int # = # 5 do while !(!n = 0) do n := !n - 1; c := !a; a := !b; b := !b + !c; end end end; !b end end" of
    VInt 8 -> Right ()
    r -> Left $ "test_iterfib FAIL, got " ++ show r

test_higherorder :: TestCase
test_higherorder e = case e "with f : (int -> int -> int) = fun (x : int) : (int -> int) -> fun (y : int) : int -> x + y end end do f 3 5 end" of
    VInt 8 -> Right ()
    r -> Left $ "test_higherorder FAIL, got " ++ show r

test_91 :: TestCase
test_91 e = case e "with fun m (n : int) : int = with n < 101 case | true -> m (m (n + 11)) | false -> n - 10 end do with a : int # = # 0 do while m !a = 91 do a := !a + 1 end; !a end end" of
    VInt 102 -> Right ()
    r -> Left $ "test_91 fail, got " ++ show r

test_sqrt :: TestCase
test_sqrt e = case e "with fun sqrt (x : int) : int = with fun sqrtHelper (x : int) : (int -> int) = fun (y : int) : int -> with z : int = 2 * y - 1 do with z < x+1 case | true -> sqrtHelper (x-z) (y+1) | false -> y-1 end end end do sqrtHelper x 1 end do sqrt 60 end" of
    VInt 7 -> Right ()
    r -> Left $ "test_sqrt fail, got " ++ show r

runCodeTest :: IO ()
runCodeTest =
    let allTests = do
            test_arith
            test_lambda
            test_withfun
            test_casematch
            test_fib
            test_ref
            test_iterfib
            test_higherorder
            test_91
            test_sqrt
    in do
        lr <- loadParserFrom "data/lr1.txt"
        case allTests (fst . eval lr) >> allTests (getValue . toCPS . fst . getAST lr) of
            Left err -> error err
            Right _ -> putStrLn "LuminaCodeTest PASS"
