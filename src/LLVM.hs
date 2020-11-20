{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecursiveDo #-}
module LLVM (
  toIRNes
) where

import AddressSpace(AddressSpace, resetAddress)
import qualified Assembly as I
import Control.Monad (forM_)
import Data.Maybe (fromJust)
import Data.Word (Word8, Word16)
import LLVM.AST hiding (function)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.IntegerPredicate as P
import qualified LLVM.AST.Linkage as L
import LLVM.AST.Type (i8, i16, ptr)
import LLVM.IRBuilder
import PyF (fmt)

stateStructDef :: Definition
stateStructDef = TypeDefinition "struct.state" $ Just StructureType {
    isPacked = False,
    elementTypes = [
      ArrayType 12 i8, -- registers
      i8, -- next action to take
      i16, -- optional address argument for action
      i8, -- optional value argument for action
      i8, -- optional register argument for action
      ptr continuationFunction, -- function to resume
      ArrayType 65536 i8
    ]
  }

stateStruct :: Type
stateStruct = NamedTypeReference "struct.state"

getOperation :: Operand -> IRBuilder Operand
getOperation state = do
  addr <- emitInstr (ptr i8) $ GetElementPtr True state [ConstantOperand $ C.Int 32 0,
            ConstantOperand $ C.Int 32 1] []
  val <- load addr 0
  return val

setOperation :: Operand -> Operand -> IRBuilder ()
setOperation state val = do
  addr <- emitInstr (ptr i8) $ GetElementPtr True state [ConstantOperand $ C.Int 32 0,
            ConstantOperand $ C.Int 32 1] []
  store addr 0 val

getReg :: Operand -> IRBuilder Operand
getReg state = do
  addr <- emitInstr (ptr i8) $ GetElementPtr True state [ConstantOperand $ C.Int 32 0,
            ConstantOperand $ C.Int 32 4] []
  val <- load addr 0
  return val

setReg :: Operand -> Operand -> IRBuilder ()
setReg state val = do
  addr <- emitInstr (ptr i8) $ GetElementPtr True state [ConstantOperand $ C.Int 32 0,
            ConstantOperand $ C.Int 32 4] []
  store addr 0 val

getRegValue :: Operand -> Operand -> IRBuilder Operand
getRegValue state reg = do
  addr <- emitInstr (ptr i8) $ GetElementPtr True state [ConstantOperand $ C.Int 32 0,
            ConstantOperand $ C.Int 32 0, reg] []
  val <- load addr 0
  return val

setRegValue :: Operand -> Operand -> Operand -> IRBuilder ()
setRegValue state reg val = do
  addr <- emitInstr (ptr i8) $ GetElementPtr True state [ConstantOperand $ C.Int 32 0,
            ConstantOperand $ C.Int 32 0, reg] []
  store addr 0 val

getCont :: Operand -> IRBuilder Operand
getCont state = do
  addr <- emitInstr (ptr $ ptr continuationFunction) $ GetElementPtr True state [ConstantOperand $ C.Int 32 0,
            ConstantOperand $ C.Int 32 5] []
  val <- load addr 0
  return val

setCont :: Operand -> Operand -> IRBuilder ()
setCont state cont = do
  addr <- emitInstr (ptr $ ptr continuationFunction) $ GetElementPtr True state [ConstantOperand $ C.Int 32 0,
            ConstantOperand $ C.Int 32 5] []
  store addr 0 cont

getMemAddr :: Operand -> IRBuilder Operand
getMemAddr state = do
  addr <- emitInstr (ptr i16) $ GetElementPtr True state [ConstantOperand $ C.Int 32 0,
            ConstantOperand $ C.Int 32 2] []
  val <- load addr 0
  return val

setMemAddr :: Operand -> Operand -> IRBuilder ()
setMemAddr state addr = do
  addr' <- emitInstr (ptr i16) $ GetElementPtr True state [ConstantOperand $ C.Int 32 0,
            ConstantOperand $ C.Int 32 2] []
  store addr' 0 addr

getStoreMemValue :: Operand -> IRBuilder Operand
getStoreMemValue state = do
  addr <- emitInstr (ptr i8) $ GetElementPtr True state [ConstantOperand $ C.Int 32 0,
            ConstantOperand $ C.Int 32 3] []
  val <- load addr 0
  return val

setStoreMemValue :: Operand -> Operand -> IRBuilder ()
setStoreMemValue state addr = do
  addr' <- emitInstr (ptr i8) $ GetElementPtr True state [ConstantOperand $ C.Int 32 0,
            ConstantOperand $ C.Int 32 3] []
  store addr' 0 addr

getMemValue :: Operand -> Operand -> IRBuilder Operand
getMemValue state addr = do
  addr <- emitInstr (ptr i8) $ GetElementPtr True state [ConstantOperand $ C.Int 32 0,
            ConstantOperand $ C.Int 32 6, addr] []
  val <- load addr 0
  return val

setMemValue :: Operand -> Operand -> Operand -> IRBuilder ()
setMemValue state addr val = do
  addr <- emitInstr (ptr i8) $ GetElementPtr True state [ConstantOperand $ C.Int 32 0,
            ConstantOperand $ C.Int 32 6, addr] []
  store addr 0 val

continuationFunction :: Type
continuationFunction = FunctionType {
  resultType = VoidType,
  argumentTypes = [ptr stateStruct],
  isVarArg = False
}

nesReadMemDef :: Definition
nesReadMemDef = GlobalDefinition $ functionDefaults {
  G.name = "readMem",
  G.parameters = ([Parameter (ptr stateStruct) "state" [], Parameter i16 "addr" []], False),
  G.returnType = VoidType,
  G.linkage = L.Private,
  G.basicBlocks = body
  }
  where state = LocalReference (ptr stateStruct) "state"
        addr = LocalReference i16 "addr"
        body = execIRBuilder emptyIRBuilder $ mdo
          _entry <- block `named` "entry"
          condZero <- icmp P.ULT addr $ literalAddr 0x100
          condBr condZero ifZero elseZero
          ifZero <- block `named` "ifZero"
          resumeRead state $ literal 0
          elseZero <- block `named` "elseZero"
          condRam <- icmp P.ULT addr $ literalAddr 0x800
          condBr condRam ifRam elseRam
          ifRam <- block `named` "ifRam"
          ramMemVal <- getMemValue state addr
          resumeRead state ramMemVal
          elseRam <- block `named` "elseRam"
          condLow <- icmp P.ULT addr $ literalAddr 0x2000
          condBr condLow ifLow elseLow
          ifLow <- block `named` "ifLow"
          ifLowLoc <- urem addr $ literalAddr 0x800
          readMem state ifLowLoc
          elseLow <- block `named` "elseLow"
          condPpu <- icmp P.ULT addr $ literalAddr 0x4000
          condBr condPpu ifPpu elsePpu
          ifPpu <- block `named` "ifPpu"
          ppuLocTemp <- sub addr $ literalAddr 0x2000
          ppuLocTemp2 <- urem ppuLocTemp $ literalAddr 0x100
          ppuLoc <- add ppuLocTemp2 $ literalAddr 0x2000
          setOperation state $ literal 0
          setMemAddr state ppuLoc
          retVoid
          elsePpu <- block `named` "elsePpu"
          condApu <- icmp P.ULT addr $ literalAddr 0x4020
          condBr condApu ifApu end
          ifApu <- block `named` "ifApu"
          setOperation state $ literal 0
          setMemAddr state addr
          retVoid
          end <- block `named` "end"
          memVal <- getMemValue state addr
          resumeRead state memVal

readMem :: Operand -> Operand -> IRBuilder ()
readMem state addr = do
  call func [(state, []), (addr, [])]
  retVoid
  where func = ConstantOperand $ C.GlobalReference FunctionType {
    resultType = VoidType,
    argumentTypes = [ptr stateStruct, i16],
    isVarArg = False
  } "readMem"

nesWriteMemDef :: Definition
nesWriteMemDef = GlobalDefinition $ functionDefaults {
  G.name = "writeMem",
  G.parameters = ([Parameter (ptr stateStruct) "state" [], Parameter i16 "addr" [],
                   Parameter i8 "val" []], False),
  G.returnType = VoidType,
  G.linkage = L.Private,
  G.basicBlocks = body
  }
  where state = LocalReference (ptr stateStruct) "state"
        addr = LocalReference i16 "addr"
        val = LocalReference i8 "val"
        body = execIRBuilder emptyIRBuilder $ mdo
          _entry <- block `named` "entry"
          condZero <- icmp P.ULT addr $ literalAddr 0x100
          condBr condZero ifZero elseZero
          ifZero <- block `named` "ifZero"
          resume state
          elseZero <- block `named` "elseZero"
          condRam <- icmp P.ULT addr $ literalAddr 0x800
          condBr condRam ifRam elseRam
          ifRam <- block `named` "ifRam"
          setMemValue state addr val
          resume state
          elseRam <- block `named` "elseRam"
          condLow <- icmp P.ULT addr $ literalAddr 0x2000
          condBr condLow ifLow elseLow
          ifLow <- block `named` "ifLow"
          ifLowLoc <- urem addr $ literalAddr 0x800
          readMem state ifLowLoc
          elseLow <- block `named` "elseLow"
          condPpu <- icmp P.ULT addr $ literalAddr 0x4000
          condBr condPpu ifPpu elsePpu
          ifPpu <- block `named` "ifPpu"
          ppuLocTemp <- sub addr $ literalAddr 0x2000
          ppuLocTemp2 <- urem ppuLocTemp $ literalAddr 0x100
          ppuLoc <- add ppuLocTemp2 $ literalAddr 0x2000
          setOperation state $ literal 1
          setMemAddr state ppuLoc
          setStoreMemValue state val
          retVoid
          elsePpu <- block `named` "elsePpu"
          condApu <- icmp P.ULT addr $ literalAddr 0x4020
          condBr condApu ifApu end
          ifApu <- block `named` "ifApu"
          setOperation state $ literal 1
          setMemAddr state addr
          setStoreMemValue state val
          retVoid
          end <- block `named` "end"
          setMemValue state addr val
          resume state

writeMem :: Operand -> Operand -> Operand -> IRBuilder ()
writeMem state addr val = do
  call func [(state, []), (addr, []), (val, [])]
  retVoid
  where func = ConstantOperand $ C.GlobalReference FunctionType {
    resultType = VoidType,
    argumentTypes = [ptr stateStruct, i16, i8],
    isVarArg = False
  } "writeMem"

resumeReadDef :: Definition
resumeReadDef = GlobalDefinition $ functionDefaults {
  G.name = "resumeRead",
  G.parameters = ([Parameter (ptr stateStruct) "state" [], Parameter i8 "val" []], False),
  G.returnType = VoidType,
  G.basicBlocks = body
  }
  where state = LocalReference (ptr stateStruct) "state"
        val = LocalReference i8 "val"
        body = execIRBuilder emptyIRBuilder $ mdo
          block `named` "entry"
          reg <- getReg state
          setRegValue state reg val
          resume state
          retVoid

resumeRead :: Operand -> Operand -> IRBuilder ()
resumeRead state val = do
  call func [(state, []), (val, [])]
  retVoid
  where func = ConstantOperand $ C.GlobalReference FunctionType {
    resultType = VoidType,
    argumentTypes = [ptr stateStruct, i8],
    isVarArg = False
  } "resumeRead"

resumeDef :: Definition
resumeDef = GlobalDefinition $ functionDefaults {
  G.name = "resume",
  G.parameters = ([Parameter (ptr stateStruct) "state" []], False),
  G.returnType = VoidType,
  G.basicBlocks = body
  }
  where state = LocalReference (ptr stateStruct) "state"
        body = execIRBuilder emptyIRBuilder $ mdo
          cont <- getCont state
          call cont [(state, [])]
          retVoid

resume :: Operand -> IRBuilder ()
resume state = do
  call func [(state, [])]
  retVoid
  where func = ConstantOperand $ C.GlobalReference FunctionType {
    resultType = VoidType,
    argumentTypes = [ptr stateStruct, ptr continuationFunction],
    isVarArg = False
  } "resume"

resetDef :: AddressSpace -> Definition
resetDef mem = GlobalDefinition $ functionDefaults {
  G.name = "reset",
  G.parameters = ([Parameter (ptr stateStruct) "state" []], False),
  G.returnType = VoidType,
  G.basicBlocks = body
  }
  where state = LocalReference (ptr stateStruct) "state"
        body = execIRBuilder emptyIRBuilder $ mdo
          call (functionAtAddr $ resetAddress mem) [(state, [])]
          retVoid

nesModuleDefs :: [Definition]
nesModuleDefs = [nesReadMemDef, nesWriteMemDef]

moduleDefs :: [Definition]
moduleDefs = [stateStructDef, resumeReadDef, resumeDef]

toIRNes :: AddressSpace -> Module
toIRNes mem = defaultModule {
  moduleName = "nes",
  moduleDefinitions = moduleDefs ++ nesModuleDefs ++ [resetDef mem, toIRFunction 0x8000 mem, toIRFunction 0x8007 mem,
    toIRFunction 0x800d mem]
  }

toIRFunction :: Word16 -> AddressSpace -> Definition
toIRFunction addr mem = GlobalDefinition $ functionDefaults {
  G.name = [fmt|func_{addr:04x}|],
  G.parameters = ([Parameter (ptr stateStruct) "state" []], False),
  G.returnType = VoidType,
  G.linkage = L.Private,
  G.basicBlocks = body
  }
  where state = LocalReference (ptr stateStruct) "state"
        insts = I.functionBody addr mem
        body = execIRBuilder emptyIRBuilder $ mdo
          block `named` "entry"
          br [fmt|lbl_{addr:04x}_0|]
          forM_ insts $ toIR state

regA :: Operand
regA = literal 0

regX :: Operand
regX = literal 1

regY :: Operand
regY = literal 2

regS :: Operand
regS = literal 3

regSp :: Operand
regSp = literal 4

regN :: Operand
regN = literal 5

toIR :: Operand -> I.Instruction -> IRBuilder ()
toIR state inst = do
  let off = I.offset inst
  block `named` [fmt|lbl_{off:04x}|]
  toIR_ state inst

toIR_ :: Operand -> I.Instruction -> IRBuilder ()
toIR_ state inst@(I.Absolute _ I.LDA arg) = do
  setReg state regA
  setCont state $ contNext inst
  readMem state (literalAddr arg)
toIR_ state inst@(I.Absolute _ I.STA arg) = do
  val <- getRegValue state regA
  setCont state $ contNext inst
  writeMem state (literalAddr arg) val
toIR_ state inst@(I.Immediate _ I.LDA arg) = setRegValue state regA (literal arg) >> brNext inst
toIR_ state inst@(I.Immediate _ I.LDX arg) = setRegValue state regX (literal arg) >> brNext inst
toIR_ state inst@(I.Implied _ I.CLD) = brNext inst
toIR_ state inst@(I.Implied _ I.PLA) = brNext inst -- TODO
toIR_ state inst@(I.Implied _ I.TXS) = do
  x <- getRegValue state regX
  setRegValue state regS x
  brNext inst
toIR_ state inst@(I.Relative _ I.BPL arg) = do
  x <- getRegValue state regN
  cond <- icmp P.EQ x $ literal 0
  let next = I.nextAddr inst
  let branch = fromJust $ I.localBranch inst
  condBr cond [fmt|lbl_{branch:04x}_0|] [fmt|lbl_{next:04x}_0|] 

literal :: Word8 -> Operand
literal = ConstantOperand . C.Int 8 . fromIntegral

literalAddr :: Word16 -> Operand
literalAddr = ConstantOperand . C.Int 16 . fromIntegral

functionAtAddr :: Word16 -> Operand
functionAtAddr addr = ConstantOperand $ C.GlobalReference (ptr continuationFunction) [fmt|func_{addr:04x}|]

contNext :: I.Instruction -> Operand
contNext = functionAtAddr . I.nextAddr

brNext :: I.Instruction -> IRBuilder ()
brNext inst = br [fmt|lbl_{next:04x}_0|]
  where next = I.nextAddr inst