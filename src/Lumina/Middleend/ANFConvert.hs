module Lumina.Middleend.ANFConvert where
import Lumina.Frontend.LuminaAST (UnaryOp, BinaryOp)

{-
 - After hoisting all of our functions into a main environment and converting
 - to CPS, our expressions are actually very close to Administrative Normal Form
 - (ANF). The only exception are the primitive operators, which we will separate
 - here. Our job is hence to parse the AST and convert it to ANF.
 - 
 - ANF is also known as Monadic Normal Form.
 -
 - Note that apart from if statements, this IR is almost entirely linear.
 - 
 - In an ideal world I would have gone directly to this form but I have to demo
 - CPS form first.
 -}

data MAtom 
    = MVar String
    | MInt Int
    | MBool Bool
    | MUnit

data MValue
    = MJust MAtom
    | MUnary UnaryOp MAtom
    | MBinary BinaryOp MAtom MAtom

data MExpr
    = MLet String MValue MExpr
    | MLetApp String MValue MValue MExpr
    | MIf MValue MExpr MExpr