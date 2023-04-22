module Lumina.Backend.CeliaToX86 where
import Data.List (intercalate)

{-
 - Translate fairly crudely from Celia to x86_64 assembly.
 -
 - The calling convention differs slightly; I assume all arguments are stored
 - sequentially on the stack.
 -}

data Reg
    = RAX | RBX | RCX | RDX | RSP | RBP | RDI | RSI | R8 | R9 | R10 | R11

-- Generally, the destination register goes first
data Instr
    = Mov Reg Reg
    | LoadImm Reg Int
    | Load Reg Reg Int
    | StoreImm Reg Int Int
    | Store Reg Int Reg
    | Call String
    | CallFP Reg Int
    | Add Reg Reg
    | Sub Reg Reg
    | Mul Reg Reg
    | Xor Reg Reg
    | Cmp Reg Reg
    | SetEAL
    | Jmp String
    | JE String
    | Label String
    | Push Reg
    | Pop Reg
    | Leave
    | Ret

instance Show Reg where
    show RAX = "%rax"
    show RBX = "%rbx"
    show RCX = "%rcx"
    show RDX = "%rdx"
    show RSP = "%rsp"
    show RBP = "%rbp"
    show RDI = "%rdi"
    show RSI = "%rsi"
    show R8 = "%r8"
    show R9 = "%r9"
    show R10 = "%r10"
    show R11 = "%r11"

showImm :: Int -> String
showImm i = "$" ++ show i

showAddr :: Reg -> Int -> String
showAddr ra 0 = "(" ++ show ra ++ ")"
showAddr ra o = show o ++ "(" ++ show ra ++ ")"

showInstr :: String -> [String] -> String
showInstr opcode []       = "  " ++ opcode
showInstr opcode operands = "  " ++ opcode ++ " " ++ intercalate ", " operands

toATT :: Instr -> String
toATT (Mov rd rs) = showInstr "movq" [show rs, show rd]
toATT (LoadImm rd i) = showInstr "movq" [showImm i, show rd]
toATT (Load rd ra o) = showInstr "movq" [showAddr ra o, show rd]
toATT (StoreImm ra o i) = showInstr "movq" [showImm i, showAddr ra o]
toATT (Store ra o rs) = showInstr "movq" [show rs, showAddr ra o]
toATT (Call l) = showInstr "call" [l]
toATT (CallFP ra o) = showInstr "call" [showAddr ra o]
toATT (Add rd rs) = showInstr "add" [show rs, show rd]
toATT (Sub rd rs) = showInstr "sub" [show rs, show rd]
toATT (Mul rd rs) = showInstr "mul" [show rs, show rd]
toATT (Xor rd rs) = showInstr "xor" [show rs, show rd]
toATT (Cmp ra rb) = showInstr "cmp" [show ra, show rb]
toATT SetEAL = showInstr "sete" ["%al"]
toATT (Jmp l) = showInstr "jmp" [l]
toATT (JE l) = showInstr "le" [l]
toATT (Label l) = l ++ ":"
toATT (Push rs) = showInstr "push" [show rs]
toATT (Pop rd) = showInstr "pop" [show rd]
toATT Leave = showInstr "leave" []
toATT Ret = showInstr "ret" []

