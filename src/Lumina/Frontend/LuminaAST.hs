{-# LANGUAGE GADTs, KindSignatures #-}

module Lumina.Frontend.LuminaAST where

import Lumina.Frontend.LuminaGrammar (PAST)
import Data.Map (Map)
import qualified Data.Map as Map

newtype StoreAddress = StoreAddress Int
type Store = Map StoreAddress StoreVar

data StoreVar
    = IntVar Int
    | BoolVar Bool
    | UnitVar
    | RefVar StoreAddress
    | FunVar ((StoreVar, StoreAddress) -> (StoreVar, StoreAddress))

newtype VarRef a = VarRef a

data AST
    = BZero
    | BTrue
    | BFalse
    | BUnit
    | BVar String
    | BInt Int
    | BApp AST AST
    | BNot AST
    | BBang AST
    | BRef AST
    | BMul AST AST
    | BAnd AST AST
    | BOr AST AST
    | BSub AST AST
    | BAdd AST AST
    | BLessThan AST AST
    | BEqual AST AST
    | BAssign AST AST
    | BCase AST [(AST, AST)]
    | BFun AST AST AST AST
    | BLetFun AST AST AST AST AST AST
    | BWhile AST AST
    | BSeq AST AST
    deriving (Eq, Show)

translate :: PAST -> AST
translate = undefined