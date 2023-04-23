module Lumina.Backend.CeliaToX86 (Reg(..), ArithOp(..), CompareOp(..), Instr(..), toATT, celiaToASM, showASM) where
import Data.List (intercalate)
import Lumina.Middleend.Celia.Celia (CInstr (..), CVal (..), CLoc, CType (..), CFunction (..), CFunctionDecl (..), CBlock (..), CBlockEnd (CReturn, CGoto, CBranch), CeliaTranslationUnit)
import Control.Monad.Trans.RWS.CPS (RWS, ask, tell, withRWS, runRWS)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Lumina.Utils (internalError, countUp)
import Lumina.Middleend.Astra.Astra (BinaryOp(..))
import Control.Monad (forM_, when)
import Lumina.Backend.CeliaToC (getFlags)

{-
 - Translate fairly crudely from Celia to x86_64 assembly.
 -
 - The calling convention differs slightly; I assume all arguments are stored
 - sequentially on the stack.
 -}

data Reg
    = RAX | RBX | RCX | RDX | RSP | RBP | RDI | RSI | R8 | R9 | R10 | R11

data ArithOp = Add | Sub | Mul | Xor
data CompareOp = Equal | Less

type ASMTranslationUnit = Map String [Instr]

-- Generally, the destination register goes first
data Instr
    = Mov Reg Reg
    | LoadImm Reg Int
    | Load Reg Reg Int
    | LEA Reg String
    | StoreImm Reg Int Int
    | Store Reg Int Reg
    | Call String
    | CallFP Reg Int
    | Arith ArithOp Reg Reg
    | ArithImm ArithOp Reg Int
    | Cmp Reg Reg
    | CmpZero Reg
    | SetAL CompareOp
    | MovZXRAXAL -- lol
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

showAO :: ArithOp -> String
showAO Add = "add"
showAO Sub = "sub"
showAO Mul = "mul"
showAO Xor = "xor"

toATT :: Instr -> String
toATT (Mov rd rs) = showInstr "movq" [show rs, show rd]
toATT (LoadImm rd i) = showInstr "movq" [showImm i, show rd]
toATT (Load rd ra o) = showInstr "movq" [showAddr ra o, show rd]
toATT (LEA rd l) = showInstr "leaq" [l, show rd]
toATT (StoreImm ra o i) = showInstr "movq" [showImm i, showAddr ra o]
toATT (Store ra o rs) = showInstr "movq" [show rs, showAddr ra o]
toATT (Call l) = showInstr "call" [l]
toATT (CallFP ra o) = showInstr "call" [showAddr ra o]
toATT (Arith ao rd rs) = showInstr (showAO ao) [show rs, show rd]
toATT (ArithImm ao rd i) = showInstr (showAO ao) [showImm i, show rd]
toATT (Cmp ra rb) = showInstr "cmp" [show ra, show rb]
toATT (CmpZero ra) = showInstr "cmp" [show ra, showImm 0]
toATT (SetAL Equal) = showInstr "sete" ["%al"]
toATT (SetAL Less) = showInstr "sete" ["%al"]
toATT MovZXRAXAL = showInstr "movzbq" ["%al", "%rax"]
toATT (Jmp l) = showInstr "jmp" [l]
toATT (JE l) = showInstr "le" [l]
toATT (Label l) = l ++ ":"
toATT (Push rs) = showInstr "pushq" [show rs]
toATT (Pop rd) = showInstr "popq" [show rd]
toATT Leave = showInstr "leave" []
toATT Ret = showInstr "ret" []

-- Mapping from local variable to location in memory
type StackEnv = Map CLoc Int
-- Wow look at me, finally using the RWS monad
type AsmBuilder = RWS StackEnv [Instr] ()

data LocOrImm = Loc Int | Imm Int
getLocOrImm :: CVal -> AsmBuilder LocOrImm
getLocOrImm (CInt i) = return (Imm i)
getLocOrImm (CBool True) = return (Imm 1)
getLocOrImm (CBool False) = return (Imm 0)
getLocOrImm (CVar v) = do
    env <- ask
    return (Loc $ env Map.! v)

emit :: Instr -> AsmBuilder ()
emit i = tell [i]

loadTo :: Reg -> CVal -> AsmBuilder ()
loadTo r cv = do
    li <- getLocOrImm cv
    case li of
        Loc n -> emit $ Load r RBP n
        Imm n -> emit $ LoadImm r n

getInd :: CLoc -> AsmBuilder Int
getInd cl = do
    env <- ask
    return $ env Map.! cl

emitArithOp :: BinaryOp -> AsmBuilder ()
emitArithOp OpAdd = do
    emit $ Arith Add RAX RBX
emitArithOp OpSub = do
    emit $ Arith Sub RAX RBX
emitArithOp OpMul = do
    emit $ Arith Mul RAX RBX
emitArithOp OpLessThan = do
    emit $ Cmp RAX RBX
    emit $ SetAL Less
    emit MovZXRAXAL
emitArithOp OpIntEqual = do
    emit $ Cmp RAX RBX
    emit $ SetAL Equal
    emit MovZXRAXAL
emitArithOp OpBoolEqual = do
    emit $ Cmp RAX RBX
    emit $ SetAL Equal
    emit MovZXRAXAL
emitArithOp bo = internalError ("Unsupported BinaryOp: " ++ show bo)

storeToLoc :: CLoc -> Reg -> AsmBuilder ()
storeToLoc cl r = do
    ind <- getInd cl
    emit $ Store RBP ind r

toBinary :: String -> Int
toBinary = toBinaryRev . reverse
    where
        toBinaryRev [] = 0
        toBinaryRev ('0':xs) = toBinary xs
        toBinaryRev ('1':xs) = 2 + toBinary xs + 1
        toBinaryRev (x:_) = internalError ("Not a binary digit: " ++ show x)

emitCInstr :: CInstr -> AsmBuilder ()
emitCInstr instr = case instr of
    CLoad dst cv -> do
        loadTo R10 cv
        storeToLoc dst R10
    CBinaryOp bo dst cv cv' -> do
        loadTo RAX cv
        loadTo RBX cv'
        emitArithOp bo
        storeToLoc dst RAX
    CNot dst cv -> do
        loadTo RAX cv
        emit $ CmpZero RAX
        emit $ SetAL Equal
        emit MovZXRAXAL
        storeToLoc dst RAX
    CDeRef dst cv _ -> do
        loadTo RDI cv
        emit $ Call "deref"
        storeToLoc dst RAX
    CMkRef dst cv ct -> do
        let flag = if ct == CTPtr then 1 else 0
        emit $ LoadImm RDI flag
        loadTo RSI cv
        emit $ Call "mk_ref"
        storeToLoc dst RAX
    CCall dst f cvs cv -> do
        let args = reverse $ cv : cvs
        forM_ args $ \arg -> do
            loadTo RAX arg
            emit $ Push RAX
        emit $ Call ("f_" ++ f)
        storeToLoc dst RAX
    CCallCl _ dst cl cv -> do
        loadTo RDI (CVar cl)
        -- Load closure data (position 4 onwards) to RDI
        emit $ ArithImm Add RDI (8*4)
        -- Load argument to RSI
        loadTo RSI cv
        -- Call function pointer (position 3)
        emit $ CallFP RDI (-8)
        storeToLoc dst RAX
    CMkCl dst f tvs -> do
        -- Call alloc_closure, then move each element onto the data
        let flags = getFlags tvs
            args = fst <$> tvs
        emit $ LoadImm RDI (length flags)
        emit $ LoadImm RSI (toBinary flags)
        emit $ Call "alloc_closure"
        storeToLoc dst RAX
        -- Mov RDI in anticipation of init_closure
        emit $ Mov RDI RAX
        -- Store the function pointer
        emit $ LEA R10 ("cl_f_" ++ f)
        emit $ Store RDI (8*3) R10
        -- Store each argument
        forM_ (countUp args) $ \(i, arg) -> do
            loadTo R10 arg
            emit $ Store RDI (8 * (4+i)) R10
        -- Call init_closure to start reference counting
        emit $ Call "init_closure"
    CSet dst cv -> do
        loadTo RDI (CVar dst)
        loadTo RSI cv
        emit $ Call "set_ref"
    CIncRef dst -> do
        loadTo RDI (CVar dst)
        emit $ Call "inc_ref"
    CDecRef dst -> do
        loadTo RDI (CVar dst)
        emit $ Call "dec_ref"

emitBlockEnd :: String -> CBlockEnd -> AsmBuilder ()
emitBlockEnd _ (CGoto l) = emit $ Jmp l
emitBlockEnd _ (CBranch cv th el) = do
    loadTo RAX cv
    emit $ CmpZero RAX
    emit $ JE el
    emit $ Jmp th
emitBlockEnd epi (CReturn cv) = do
    loadTo RAX cv
    emit $ Jmp epi

emitFunction :: CFunction -> AsmBuilder ()
emitFunction (CFunction decl locals blocks) = do
    let (CFunctionDecl name _ args) = decl
        argMap = makeArgLoc <$> countUp args
        localMap = makeLocalLoc <$> countUp (Map.toList locals)
        frameSize = 8 * length locals
        pro = "f_" ++ name
        epi = "f_" ++ name ++ "_epi"

    emit $ Label pro
    -- Prologue
    emit $ Push RBP
    emit $ Mov RBP RSP
    when (frameSize > 0) (emit $ ArithImm Sub RSP frameSize)

    -- Body
    withRWS (\_ _ -> (Map.fromList (argMap ++ localMap), ())) $ 
        forM_ (Map.toList blocks) $ \(bn, CBlock is be) -> do
            emit $ Label bn
            forM_ is emitCInstr
            emitBlockEnd epi be

    -- Epilogue
    emit $ Label epi
    if frameSize == 0 then
        emit $ Pop RBP
    else
        emit Leave
    emit Ret
    where
        makeLocalLoc (i,loc) = (fst loc, 8 * (1+i))
        makeArgLoc (i,arg) = (fst arg, 8 * (-2-i))

celiaToASM :: CeliaTranslationUnit -> ASMTranslationUnit
celiaToASM = Map.map (getRes . emitFunction)
    where
        getRes builder = let (_,_,res) = runRWS builder Map.empty () in res

showASM :: ASMTranslationUnit -> String
showASM atu = Map.foldMapWithKey getHeadText atu ++ Map.foldMapWithKey getBodyText atu
    where
        getBodyText _ is = intercalate "\n" $ toATT <$> is
        getHeadText name _ = ".globl " ++ "f_" ++ name ++ "\n"