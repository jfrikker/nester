{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecursiveDo #-}
module LLVM (
  toIRNes
) where

import qualified Assembly as I
import Control.Monad (forM_, void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word (Word8, Word16)
import LLVM.AST hiding (function)
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.FunctionAttribute as FA
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Instruction as LI
import qualified LLVM.AST.IntegerPredicate as P
import qualified LLVM.AST.Linkage as L
import LLVM.AST.Type (i1, i8, i16, i64, ptr)
import LLVM.IRBuilder
import LLVM.Types (readCallbackType, writeCallbackType)
import qualified Mapper
import Mapper(Mapper)
import PyF (fmt)

instFunctionType :: Type
instFunctionType = FunctionType {
  resultType = VoidType,
  argumentTypes = [ptr readCallbackType, ptr writeCallbackType],
  isVarArg = False
}

regRef :: Type -> Name -> Operand
regRef type' = LocalReference (ptr type')

regA :: Operand
regA = regRef i8 "regA"

regX :: Operand
regX = regRef i8 "regX"

regY :: Operand
regY = regRef i8 "regY"

regN :: Operand
regN = regRef i1 "regN"

regZ :: Operand
regZ = regRef i1 "regZ"

regV :: Operand
regV = regRef i1 "regV"

regC :: Operand
regC = regRef i1 "regC"

regS :: Operand
regS = regRef i8 "regS"

regClk :: Operand
regClk = regRef i16 "clk"

addCarryDef :: Definition
addCarryDef = GlobalDefinition functionDefaults {
  G.name = "llvm.uadd.with.overflow.i8",
  G.returnType = StructureType {
    isPacked = False,
    elementTypes = [i8, i1]
  },
  G.parameters = ([Parameter i8 "a" [], Parameter i8 "b" []], False)
}

addCarry :: Operand -> Operand -> IRBuilder (Operand, Operand)
addCarry arg1 arg2 = do
  resPacked <- call func [(arg1, []), (arg2, [])]
  res <- extractValue resPacked [0]
  carry <- extractValue resPacked [1]
  return (res, carry)
  where func = ConstantOperand $ C.GlobalReference FunctionType {
    resultType = StructureType {
      isPacked = False,
      elementTypes = [i8, i1]
    },
    argumentTypes = [i8, i8],
    isVarArg = False
  } "llvm.uadd.with.overflow.i8"

readMemDef :: Mapper -> Definition
readMemDef mapper = GlobalDefinition $ functionDefaults {
  G.name = "readMem",
  G.parameters = ([Parameter (ptr readCallbackType) "readCallback" [], Parameter i16 "addr" [], Parameter (ptr i16) "clk" []], False),
  G.returnType = i8,
  G.linkage = L.Private,
  G.basicBlocks = body
  }
  where readCallback = LocalReference (ptr readCallbackType) "readCallback"
        addr = LocalReference i16 "addr"
        clk = LocalReference (ptr i16) "clk"
        body = execIRBuilder emptyIRBuilder $ Mapper.readBody mapper addr doReadCallback
        doReadCallback addr = do
          clk' <- load clk 0
          res <- emitInstr i8 $ Call {
            tailCallKind = Nothing,
            callingConvention = CC.C,
            returnAttributes = [],
            LI.function = Right readCallback,
            arguments = [(addr, []), (clk', [])],
            functionAttributes = [Right FA.ReadNone],
            metadata = []
          }
          store clk 0 $ literalAddr 0
          return res

readMem :: Operand -> IRBuilder Operand
readMem addr = call func [(LocalReference (ptr readCallbackType) "readCallback", []), (addr, []), (regClk, [])]
  where func = ConstantOperand $ C.GlobalReference FunctionType {
    resultType = i8,
    argumentTypes = [ptr readCallbackType, i16],
    isVarArg = False
  } "readMem"

writeMemDef :: Mapper -> Definition
writeMemDef mapper = GlobalDefinition $ functionDefaults {
  G.name = "writeMem",
  G.parameters = ([Parameter (ptr writeCallbackType) "writeCallback" [], Parameter i16 "addr" [],
                   Parameter i8 "val" [], Parameter (ptr i16) "clk" []], False),
  G.returnType = VoidType,
  G.linkage = L.Private,
  G.basicBlocks = body
  }
  where writeCallback = LocalReference (ptr writeCallbackType) "writeCallback"
        addr = LocalReference i16 "addr"
        val = LocalReference i8 "val"
        clk = LocalReference (ptr i16) "clk"
        body = execIRBuilder emptyIRBuilder $ Mapper.writeBody mapper addr val doWriteCallback
        doWriteCallback addr val = do
          clk' <- load clk 0
          emitInstrVoid $ Call {
            tailCallKind = Nothing,
            callingConvention = CC.C,
            returnAttributes = [],
            LI.function = Right writeCallback,
            arguments = [(addr, []), (val, []), (clk', [])],
            functionAttributes = [Right FA.ReadNone],
            metadata = []
          }
          store clk 0 $ literalAddr 0

writeMem :: Operand -> Operand -> IRBuilder ()
writeMem addr val = do void $ call func [(LocalReference (ptr writeCallbackType) "writeCallback", []), (addr, []), (val, []), (regClk, [])]
  where func = ConstantOperand $ C.GlobalReference FunctionType {
    resultType = VoidType,
    argumentTypes = [ptr writeCallbackType, i16, i8],
    isVarArg = False
  } "writeMem"

mapperIdDef :: Mapper -> Definition
mapperIdDef mapper = GlobalDefinition $ functionDefaults {
  G.name = "mapperId",
  G.parameters = ([], False),
  G.returnType = i8,
  G.basicBlocks = body
  }
  where body = execIRBuilder emptyIRBuilder $ ret (literal $ Mapper.mapperId mapper)

resetDef :: Mapper -> Definition
resetDef mem = GlobalDefinition $ functionDefaults {
  G.name = "reset",
  G.parameters = ([Parameter (ptr readCallbackType) "readCallback" [],
                   Parameter (ptr writeCallbackType) "writeCallback" []], False),
  G.returnType = VoidType,
  G.basicBlocks = body
  }
  where rcb = LocalReference (ptr readCallbackType) "readCallback"
        wcb = LocalReference (ptr writeCallbackType) "writeCallback"
        body = execIRBuilder emptyIRBuilder $ do
          a <- alloca i8 Nothing 0
          x <- alloca i8 Nothing 0
          y <- alloca i8 Nothing 0
          n <- alloca i1 Nothing 0
          z <- alloca i1 Nothing 0
          v <- alloca i1 Nothing 0
          c <- alloca i1 Nothing 0
          s <- alloca i8 Nothing 0
          clk <- alloca i16 Nothing 0
          store clk 0 $ literalAddr 0
          call (functionAtAddr $ Mapper.resetAddress mem) [(rcb, []), (wcb, []), (a, []), (x, []), (y, []), (n, []),
            (z, []), (v, []), (c, []), (s, []), (clk, [])]
          retVoid

nmiDef :: Mapper -> Definition
nmiDef mem = GlobalDefinition $ functionDefaults {
  G.name = "nmi",
  G.parameters = ([Parameter (ptr readCallbackType) "readCallback" [],
                   Parameter (ptr writeCallbackType) "writeCallback" []], False),
  G.returnType = VoidType,
  G.basicBlocks = body
  }
  where rcb = LocalReference (ptr readCallbackType) "readCallback"
        wcb = LocalReference (ptr writeCallbackType) "writeCallback"
        body = execIRBuilder emptyIRBuilder $ do
          a <- alloca i8 Nothing 0
          x <- alloca i8 Nothing 0
          y <- alloca i8 Nothing 0
          n <- alloca i1 Nothing 0
          z <- alloca i1 Nothing 0
          v <- alloca i1 Nothing 0
          c <- alloca i1 Nothing 0
          s <- alloca i8 Nothing 0
          clk <- alloca i16 Nothing 0
          store clk 0 $ literalAddr 0
          call (functionAtAddr $ Mapper.nmiAddress mem) [(rcb, []), (wcb, []), (a, []), (x, []), (y, []), (n, []),
            (z, []), (v, []), (c, []), (s, []), (clk, [])]
          retVoid

moduleDefs :: [Definition]
moduleDefs = [addCarryDef]

toIRNes :: Map Word16 (Map Word16 I.Instruction) -> Mapper -> Module
toIRNes funcs mapper = defaultModule {
  moduleName = "nes",
  moduleDefinitions = moduleDefs ++ Mapper.globals mapper ++ [readMemDef mapper, writeMemDef mapper, resetDef mapper, nmiDef mapper] ++ irFuncs
  }
  where irFuncs = map (\(off, body) -> toIRFunction off $ Map.elems body) $ Map.assocs funcs

toIRFunction :: Word16 -> [I.Instruction] -> Definition
toIRFunction addr insts = GlobalDefinition $ functionDefaults {
  G.name = [fmt|func_{addr:04x}|],
  G.parameters = ([Parameter (ptr readCallbackType) "readCallback" [],
                   Parameter (ptr writeCallbackType) "writeCallback" [],
                   Parameter (ptr i8) "regA" [],
                   Parameter (ptr i8) "regX" [], Parameter (ptr i8) "regY" [],
                   Parameter (ptr i1) "regN" [], Parameter (ptr i1) "regZ" [],
                   Parameter (ptr i1) "regV" [], Parameter (ptr i1) "regC" [],
                   Parameter (ptr i8) "regS" [], Parameter (ptr i16) "clk" []], False),
  G.returnType = VoidType,
  G.linkage = L.Private,
  G.basicBlocks = body,
  G.functionAttributes = [Right FA.AlwaysInline]
  }
  where body = execIRBuilder emptyIRBuilder $ mdo
          block `named` "entry"
          alloca (ArrayType 10 i8) Nothing 0 `named` "stack"
          sp <- alloca i64 Nothing 0 `named` "sp"
          store sp 0 $ ConstantOperand $ C.Int 64 0
          br [fmt|lbl_{addr:04x}_0|]
          forM_ insts toIR

toIR :: I.Instruction -> IRBuilder ()
toIR inst = do
  let off = I.offset inst
  block `named` [fmt|lbl_{off:04x}|]
  toIR_ inst

toIR_ :: I.Instruction -> IRBuilder ()
toIR_ inst@(I.Absolute _ I.ADC arg) = do
  absoluteValue arg >>= _adc
  incrClk 4
  brNext inst
toIR_ inst@(I.Absolute _ I.AND arg) = do
  absoluteValue arg >>= _and
  incrClk 4
  brNext inst
toIR_ inst@(I.Absolute _ I.ASL arg) = do
  absoluteAddr arg >>= _asl
  incrClk 6
  brNext inst
toIR_ inst@(I.Absolute _ I.BIT arg) = do
  absoluteValue arg >>= _bit
  incrClk 4
  brNext inst
toIR_ inst@(I.Absolute _ I.CMP arg) = do
  absoluteValue arg >>= _compare regA
  incrClk 4
  brNext inst
toIR_ inst@(I.Absolute _ I.CPX arg) = do
  absoluteValue arg >>= _compare regX
  incrClk 4
  brNext inst
toIR_ inst@(I.Absolute _ I.CPY arg) = do
  absoluteValue arg >>= _compare regY
  incrClk 4
  brNext inst
toIR_ inst@(I.Absolute _ I.DEC arg) = do
  absoluteAddr arg >>= modifyMem _decrement
  incrClk 6
  brNext inst
toIR_ inst@(I.Absolute _ I.EOR arg) = do
  absoluteAddr arg >>= _eor
  incrClk 4
  brNext inst
toIR_ inst@(I.Absolute _ I.INC arg) = do
  absoluteAddr arg >>= modifyMem _increment
  incrClk 6
  brNext inst
toIR_ (I.Absolute _ I.JMP arg) = do
  incrClk 3
  br [fmt|lbl_{arg:04x}_0|]
toIR_ inst@(I.Absolute _ I.JSR arg) = do
  incrClk 6
  call (functionAtAddr arg) [(LocalReference (ptr readCallbackType) "readCallback", []),
    (LocalReference (ptr writeCallbackType) "writeCallback", []), (regA, []), (regX, []),
    (regY, []), (regN, []), (regZ, []), (regV, []), (regC, []), (regS, []), (regClk, [])]
  brNext inst
toIR_ inst@(I.Absolute _ I.LDA arg) = do
  absoluteValue arg >>= _load regA
  incrClk 4
  brNext inst
toIR_ inst@(I.Absolute _ I.LDX arg) = do
  absoluteValue arg >>= _load regX
  incrClk 4
  brNext inst
toIR_ inst@(I.Absolute _ I.LDY arg) = do
  absoluteValue arg >>= _load regY 
  incrClk 4
  brNext inst
toIR_ inst@(I.Absolute _ I.LSR arg) = do
  absoluteAddr arg >>= modifyMem _lsr
  incrClk 6
  brNext inst
toIR_ inst@(I.Absolute _ I.ORA arg) = do
  absoluteValue arg >>= _ora
  incrClk 4
  brNext inst
toIR_ inst@(I.Absolute _ I.ROL arg) = do
  absoluteValue arg >>= _rol
  incrClk 6
  brNext inst
toIR_ inst@(I.Absolute _ I.ROR arg) = do
  absoluteValue arg >>= _ror
  incrClk 6
  brNext inst
toIR_ inst@(I.Absolute _ I.SBC arg) = brNext inst -- TODO
toIR_ inst@(I.Absolute _ I.STA arg) = do
  absoluteAddr arg >>= _store regA
  incrClk 4
  brNext inst
toIR_ inst@(I.Absolute _ I.STX arg) = do
  absoluteAddr arg >>= _store regX
  incrClk 4
  brNext inst
toIR_ inst@(I.Absolute _ I.STY arg) = do
  absoluteAddr arg >>= _store regY
  incrClk 4
  brNext inst
toIR_ inst@(I.AbsoluteX _ I.ADC arg) = do
  absoluteXValue arg >>= _adc
  incrClkPageBoundaryReg arg regX
  brNext inst
toIR_ inst@(I.AbsoluteX _ I.AND arg) = absoluteXValue arg >>= _and >> brNext inst
toIR_ inst@(I.AbsoluteX _ I.ASL arg) = absoluteXAddr arg >>= _asl >> brNext inst
toIR_ inst@(I.AbsoluteX _ I.DEC arg) = absoluteXAddr arg >>= modifyMem _decrement >> brNext inst
toIR_ inst@(I.AbsoluteX _ I.EOR arg) = absoluteXAddr arg >>= _eor >> brNext inst
toIR_ inst@(I.AbsoluteX _ I.INC arg) = absoluteXAddr arg >>= modifyMem _increment >> brNext inst
toIR_ inst@(I.AbsoluteX _ I.LDA arg) = absoluteXValue arg >>= _load regA >> brNext inst
toIR_ inst@(I.AbsoluteX _ I.LDY arg) = absoluteXValue arg >>= _load regY >> brNext inst
toIR_ inst@(I.AbsoluteX _ I.LSR arg) = absoluteXAddr arg >>= modifyMem _lsr >> brNext inst
toIR_ inst@(I.AbsoluteX _ I.ORA arg) = absoluteXValue arg >>= _ora >> brNext inst
toIR_ inst@(I.AbsoluteX _ I.ROR arg) = do
  addr <- absoluteXAddr arg
  val <- readMem addr
  newVal <- _ror val
  writeMem addr newVal
  brNext inst
toIR_ inst@(I.AbsoluteX _ I.STA arg) = absoluteXAddr arg >>= _store regA >> brNext inst
toIR_ inst@(I.AbsoluteY _ I.ADC arg) = absoluteYValue arg >>= _adc >> brNext inst
toIR_ inst@(I.AbsoluteY _ I.AND arg) = absoluteYValue arg >>= _and >> brNext inst
toIR_ inst@(I.AbsoluteY _ I.EOR arg) = absoluteYValue arg >>= _eor >> brNext inst
toIR_ inst@(I.AbsoluteY _ I.LDA arg) = absoluteYValue arg >>= _load regA >> brNext inst
toIR_ inst@(I.AbsoluteY _ I.LDX arg) = absoluteYValue arg >>= _load regX >> brNext inst
toIR_ inst@(I.AbsoluteY _ I.ORA arg) = absoluteYValue arg >>= _ora >> brNext inst
toIR_ inst@(I.AbsoluteY _ I.STA arg) = absoluteYAddr arg >>= _store regA >> brNext inst
toIR_ inst@(I.Accumulator _ I.ASL) = withAccumulator _asl >> brNext inst
toIR_ inst@(I.Accumulator _ I.LSR) = withAccumulator _lsr >> brNext inst
toIR_ inst@(I.Accumulator _ I.ROL) = withAccumulator _rol >> brNext inst
toIR_ inst@(I.Accumulator _ I.ROR) = withAccumulator _ror >> brNext inst
toIR_ inst@(I.Immediate _ I.ADC arg) = immediateValue arg >>= _adc >> brNext inst
toIR_ inst@(I.Immediate _ I.AND arg) = immediateValue arg >>= _and >> brNext inst
toIR_ inst@(I.Immediate _ I.CMP arg) = immediateValue arg >>= _compare regA >> brNext inst
toIR_ inst@(I.Immediate _ I.CPX arg) = immediateValue arg >>= _compare regX >> brNext inst
toIR_ inst@(I.Immediate _ I.CPY arg) = immediateValue arg >>= _compare regY >> brNext inst
toIR_ inst@(I.Immediate _ I.EOR arg) = immediateValue arg >>= _eor >> brNext inst
toIR_ inst@(I.Immediate _ I.LDA arg) = do
  immediateValue arg >>= _load regA
  incrClk 2
  brNext inst
toIR_ inst@(I.Immediate _ I.LDX arg) = immediateValue arg >>= _load regX >> brNext inst
toIR_ inst@(I.Immediate _ I.LDY arg) = immediateValue arg >>= _load regY >> brNext inst
toIR_ inst@(I.Immediate _ I.ORA arg) = immediateValue arg >>= _ora >> brNext inst
toIR_ inst@(I.Implied _ I.CLC) = do
  store regC 0 $ ConstantOperand $ C.Int 1 0
  brNext inst
toIR_ inst@(I.Implied _ I.CLD) = do
  incrClk 2
  brNext inst
toIR_ inst@(I.Implied _ I.CLV) = do
  store regV 0 $ ConstantOperand $ C.Int 1 0
  brNext inst
toIR_ inst@(I.Implied _ I.DEX) = modifyReg _decrement regX >> brNext inst
toIR_ inst@(I.Implied _ I.DEY) = modifyReg _decrement regY >> brNext inst
toIR_ inst@(I.Implied _ I.INX) = modifyReg _increment regX >> brNext inst
toIR_ inst@(I.Implied _ I.INY) = modifyReg _increment regY >> brNext inst
toIR_ inst@(I.Implied _ I.PHA) = do
  sp <- load (LocalReference (ptr i64) "sp_0") 0
  a <- load regA 0
  stackLoc <- emitInstr (ptr i8) $ GetElementPtr True (LocalReference (ptr (ArrayType 10 i8)) "stack_0") [ConstantOperand $ C.Int 32 0, sp] []
  store stackLoc 0 a
  newSp <- add sp $ ConstantOperand $ C.Int 64 1
  store (LocalReference (ptr i64) "sp_0") 0 newSp
  brNext inst
toIR_ inst@(I.Implied _ I.PLA) = do
  sp <- load (LocalReference (ptr i64) "sp_0") 0
  newSp <- sub sp $ ConstantOperand $ C.Int 64 1
  stackLoc <- emitInstr (ptr i8) $ GetElementPtr True (LocalReference (ptr (ArrayType 10 i8)) "stack_0") [ConstantOperand $ C.Int 32 0, sp] []
  a <- load stackLoc 0
  store regA 0 a
  store (LocalReference (ptr i64) "sp_0") 0 newSp
  setNZ a
  brNext inst
toIR_ (I.Implied _ I.RTI) = retVoid
toIR_ (I.Implied _ I.RTS) = retVoid
toIR_ inst@(I.Implied _ I.SEC) = do
  store regC 0 $ ConstantOperand $ C.Int 1 1
  brNext inst
toIR_ inst@(I.Implied _ I.SEI) = do
  incrClk 2
  brNext inst
toIR_ (I.Implied _ I.SLP) = retVoid
toIR_ inst@(I.Implied _ I.TAX) = _transfer regA regX >> brNext inst
toIR_ inst@(I.Implied _ I.TAY) = _transfer regA regY >> brNext inst
toIR_ inst@(I.Implied _ I.TXA) = _transfer regX regA >> brNext inst
toIR_ inst@(I.Implied _ I.TXS) = _transfer regX regS >> brNext inst
toIR_ inst@(I.IndirectX _ I.ADC arg) = indirectXValue arg >>= _adc >> brNext inst
toIR_ inst@(I.IndirectX _ I.AND arg) = indirectXValue arg >>= _and >> brNext inst
toIR_ inst@(I.IndirectX _ I.EOR arg) = indirectXValue arg >>= _eor >> brNext inst
toIR_ inst@(I.IndirectX _ I.LDA arg) = indirectXValue arg >>= _load regA >> brNext inst
toIR_ inst@(I.IndirectX _ I.ORA arg) = indirectXValue arg >>= _ora >> brNext inst
toIR_ inst@(I.IndirectY _ I.ADC arg) = indirectYValue arg >>= _adc >> brNext inst
toIR_ inst@(I.IndirectY _ I.AND arg) = indirectYValue arg >>= _and >> brNext inst
toIR_ inst@(I.IndirectY _ I.EOR arg) = indirectYValue arg >>= _eor >> brNext inst
toIR_ inst@(I.IndirectY _ I.LDA arg) = indirectYValue arg >>= _load regA >> brNext inst
toIR_ inst@(I.IndirectY _ I.ORA arg) = indirectYValue arg >>= _ora >> brNext inst
toIR_ inst@(I.IndirectY _ I.STA arg) = indirectYAddr arg >>= _store regA >> brNext inst
toIR_ inst@(I.Relative _ I.BCC _) = do
  cond <- load regC 0
  let next = I.nextAddr inst
  let branch = I.followingAddrs inst !! 1
  condBr cond [fmt|lbl_{next:04x}_0|] [fmt|lbl_{branch:04x}_0|] 
toIR_ inst@(I.Relative _ I.BCS _) = do
  cond <- load regC 0
  let next = I.nextAddr inst
  let branch = I.followingAddrs inst !! 1
  condBr cond [fmt|lbl_{branch:04x}_0|] [fmt|lbl_{next:04x}_0|] 
toIR_ inst@(I.Relative _ I.BEQ _) = do
  cond <- load regZ 0
  let next = I.nextAddr inst
  let branch = I.followingAddrs inst !! 1
  condBr cond [fmt|lbl_{branch:04x}_0|] [fmt|lbl_{next:04x}_0|]
toIR_ inst@(I.Relative _ I.BMI _) = do
  cond <- load regN 0
  let next = I.nextAddr inst
  let branch = I.followingAddrs inst !! 1
  condBr cond [fmt|lbl_{branch:04x}_0|] [fmt|lbl_{next:04x}_0|] 
toIR_ inst@(I.Relative _ I.BNE _) = do
  cond <- load regZ 0
  let next = I.nextAddr inst
  let branch = I.followingAddrs inst !! 1
  condBr cond [fmt|lbl_{next:04x}_0|] [fmt|lbl_{branch:04x}_0|]
toIR_ inst@(I.Relative _ I.BPL _) = do
  cond <- load regN 0
  let next = I.nextAddr inst
  let branch = I.followingAddrs inst !! 1
  condBr cond [fmt|lbl_{next:04x}_0|] [fmt|lbl_{branch:04x}_0|] 
toIR_ inst@(I.Relative _ I.BVC _) = do
  cond <- load regV 0
  let next = I.nextAddr inst
  let branch = I.followingAddrs inst !! 1
  condBr cond [fmt|lbl_{next:04x}_0|] [fmt|lbl_{branch:04x}_0|]
toIR_ inst@(I.Relative _ I.BVS _) = do
  cond <- load regV 0
  let next = I.nextAddr inst
  let branch = I.followingAddrs inst !! 1
  condBr cond [fmt|lbl_{branch:04x}_0|] [fmt|lbl_{next:04x}_0|]
toIR_ (I.Switch _ I.SWA _ tgts) = do
  cond <- load regA 0
  let first = head tgts
  switch cond [fmt|lbl_{first:04x}_0|] $ zipWith (curry (\(i, t) -> (C.Int 8 i, [fmt|lbl_{t:04x}_0|]))) [0..] tgts
toIR_ inst@(I.Zeropage _ I.ADC arg) = zeropageValue arg >>= _adc >> brNext inst
toIR_ inst@(I.Zeropage _ I.AND arg) = zeropageValue arg >>= _and >> brNext inst
toIR_ inst@(I.Zeropage _ I.ASL arg) = zeropageAddr arg >>= _asl >> brNext inst
toIR_ inst@(I.Zeropage _ I.BIT arg) = zeropageValue arg >>= _bit >> brNext inst
toIR_ inst@(I.Zeropage _ I.CMP arg) = zeropageValue arg >>= _compare regA >> brNext inst
toIR_ inst@(I.Zeropage _ I.DEC arg) = zeropageAddr arg >>= modifyMem _decrement >> brNext inst
toIR_ inst@(I.Zeropage _ I.EOR arg) = zeropageValue arg >>= _eor >> brNext inst
toIR_ inst@(I.Zeropage _ I.INC arg) = zeropageAddr arg >>= modifyMem _increment >> brNext inst
toIR_ inst@(I.Zeropage _ I.LDA arg) = zeropageValue arg >>= _load regA >> brNext inst
toIR_ inst@(I.Zeropage _ I.LDX arg) = zeropageValue arg >>= _load regX >> brNext inst
toIR_ inst@(I.Zeropage _ I.LDY arg) = zeropageValue arg >>= _load regY >> brNext inst
toIR_ inst@(I.Zeropage _ I.LSR arg) = zeropageAddr arg >>= modifyMem _lsr >> brNext inst
toIR_ inst@(I.Zeropage _ I.ORA arg) = zeropageValue arg >>= _ora >> brNext inst
toIR_ inst@(I.Zeropage _ I.STA arg) = zeropageAddr arg >>= _store regA >> brNext inst
toIR_ inst@(I.Zeropage _ I.STX arg) = zeropageAddr arg >>= _store regX >> brNext inst
toIR_ inst@(I.Zeropage _ I.STY arg) = zeropageAddr arg >>= _store regY >> brNext inst
toIR_ inst@(I.ZeropageX _ I.ADC arg) = zeropageXValue arg >>= _adc >> brNext inst
toIR_ inst@(I.ZeropageX _ I.AND arg) = zeropageXValue arg >>= _and >> brNext inst
toIR_ inst@(I.ZeropageX _ I.ASL arg) = zeropageXAddr arg >>= _asl >> brNext inst
toIR_ inst@(I.ZeropageX _ I.DEC arg) = zeropageXAddr arg >>= modifyMem _decrement >> brNext inst
toIR_ inst@(I.ZeropageX _ I.EOR arg) = zeropageXAddr arg >>= _eor >> brNext inst
toIR_ inst@(I.ZeropageX _ I.INC arg) = zeropageXAddr arg >>= modifyMem _increment >> brNext inst
toIR_ inst@(I.ZeropageX _ I.LDA arg) = zeropageXValue arg >>= _load regA >> brNext inst
toIR_ inst@(I.ZeropageX _ I.LDY arg) = zeropageXValue arg >>= _load regY >> brNext inst
toIR_ inst@(I.ZeropageX _ I.LSR arg) = zeropageXValue arg >>= modifyMem _lsr >> brNext inst
toIR_ inst@(I.ZeropageX _ I.ORA arg) = zeropageXValue arg >>= _ora >> brNext inst
toIR_ inst@(I.ZeropageX _ I.STA arg) = zeropageXAddr arg >>= _store regA >> brNext inst
toIR_ inst@(I.ZeropageY _ I.LDX arg) = zeropageYValue arg >>= _load regX >> brNext inst
toIR_ inst@(I.ZeropageY _ I.STA arg) = zeropageYAddr arg >>= _store regA >> brNext inst
toIR_ I.Unknown {} = retVoid -- TODO
toIR_ inst = brNext inst

absoluteAddr arg = return $ literalAddr arg
absoluteValue arg = do
  addr <- absoluteAddr arg
  readMem addr
absoluteXAddr arg = do
  x <- load regX 0
  xExt <- zext x i16
  add xExt $ literalAddr arg
absoluteXValue arg = do
  addr <- absoluteXAddr arg
  readMem addr
absoluteYAddr arg = do
  y <- load regY 0
  yExt <- zext y i16
  add yExt $ literalAddr arg
absoluteYValue arg = do
  addr <- absoluteYAddr arg
  readMem addr
immediateValue arg = return $ literal arg
indirectXAddr arg = do
  x <- load regX 0
  addrSource <- add x $ literal arg
  addrLow <- zext addrSource i16
  low <- readMem addrLow
  addrHigh <- add addrLow $ literalAddr 1
  high <- readMem addrHigh
  lowExt <- zext low i16
  highExt <- zext high i16
  highShift <- shl highExt $ literalAddr 8
  add lowExt highShift
indirectXValue arg = do
  addr <- indirectXAddr arg
  readMem addr
indirectYAddr arg = do
  addrLow <- zext (literal arg) i16
  low <- readMem addrLow
  addrHigh <- add addrLow $ literalAddr 1
  high <- readMem addrHigh
  lowExt <- zext low i16
  highExt <- zext high i16
  highShift <- shl highExt $ literalAddr 8
  addr <- add lowExt highShift
  y <- load regY 0
  yExt <- zext y i16
  add addr yExt
indirectYValue arg = do
  addr <- indirectYAddr arg
  readMem addr
modifyMem f addr = do
  val <- readMem addr
  newVal <- f val
  writeMem addr newVal
modifyReg f reg = do
  val <- load reg 0
  newVal <- f val
  store reg 0 newVal
withAccumulator f = do
  a <- load regA 0
  newA <- f a
  store regA 0 newA
zeropageAddr arg = zext (literal arg) i16
zeropageValue arg = do
  addr <- zeropageAddr arg
  readMem addr
zeropageXAddr arg = do
  x <- load regX 0
  added <- add x (literal arg)
  zext added i16
zeropageXValue arg = do
  addr <- zeropageXAddr arg
  readMem addr
zeropageYAddr arg = do
  y <- load regY 0
  added <- add y (literal arg)
  zext added i16
zeropageYValue arg = do
  addr <- zeropageYAddr arg
  readMem addr

_adc arg = do
  c <- load regC 0
  cExt <- zext c i8
  v1 <- load regA 0
  (v2, c2) <- addCarry v1 cExt
  (v3, c3) <- addCarry v2 arg
  setNZ v3
  carry <- LLVM.IRBuilder.or c2 c3
  store regC 0 carry
_and arg = do
  a <- load regA 0
  newA <- LLVM.IRBuilder.and a arg
  store regA 0 newA
  setNZ newA
_asl val = do
  newC1 <- lshr val $ literal 7
  newC2 <- trunc newC1 i1
  newVal <- shl val $ literal 1
  store regC 0 newC2
  setNZ newVal
  return newVal
_bit val = do
  nval <- lshr val $ literal 7
  nvalTrunc <- trunc nval i1
  store regN 0 nvalTrunc
  vval <- lshr val $ literal 6
  vvalTrunc <- trunc vval i1
  store regV 0 vvalTrunc
  a <- load regA 0
  res <- LLVM.IRBuilder.and a val
  setZ res
_compare reg arg = do
  s <- load reg 0
  res <- sub s arg
  setNZ res
  carry <- icmp P.UGE s arg
  store regC 0 carry
_decrement val = do
  newVal <- sub val $ literal 1
  setNZ newVal
  return newVal
_eor val = do
  a <- load regA 0
  res <- xor a val
  store regA 0 res
  setNZ res
_increment val = do
  newVal <- add val $ literal 1
  setNZ newVal
  return newVal
_load reg val = do
  store reg 0 val
  setNZ val
_lsr val = do
  newC <- trunc val i1
  store regC 0 newC
  newVal <- lshr val $ literal 1
  setNZ newVal
  return newVal
_ora arg = do
  a <- load regA 0
  newA <- LLVM.IRBuilder.or a arg
  store regA 0 newA
  setNZ newA
_rol val = do
  c <- load regC 0
  newC1 <- lshr val $ literal 7
  newC2 <- trunc newC1 i1
  newVal1 <- shl val $ literal 1
  cExt <- sext c i8
  newVal2 <- LLVM.IRBuilder.or newVal1 cExt
  store regC 0 newC2
  setNZ newVal2
  return newVal2
_ror val = do
  c <- load regC 0
  newC <- trunc val i1
  newVal <- lshr val $ literal 1
  cExt <- sext c i8
  cExt2 <- LLVM.IRBuilder.and cExt $ literal 0x80
  newVal2 <- LLVM.IRBuilder.or newVal cExt2
  store regC 0 newC
  setNZ newVal2
  return newVal2
_store reg addr = do
  val <- load reg 0
  writeMem addr val
_transfer src tgt = do
  s <- load src 0
  store tgt 0 s
  setNZ s

literal :: Word8 -> Operand
literal = ConstantOperand . C.Int 8 . fromIntegral

literalAddr :: Word16 -> Operand
literalAddr = ConstantOperand . C.Int 16 . fromIntegral

functionAtAddr :: Word16 -> Operand
functionAtAddr addr = ConstantOperand $ C.GlobalReference (ptr instFunctionType) [fmt|func_{addr:04x}|]

brNext :: I.Instruction -> IRBuilder ()
brNext inst = br [fmt|lbl_{next:04x}_0|]
  where next = I.nextAddr inst

setNZ :: Operand -> IRBuilder ()
setNZ val = do
  setN val
  setZ val

setN :: Operand -> IRBuilder ()
setN val = do
  bit <- icmp P.UGT val $ literal 0x7f
  store regN 0 bit

setZ :: Operand -> IRBuilder ()
setZ val = do
  bit <- icmp P.EQ val $ literal 0x0
  store regZ 0 bit

incrClk :: Word16 -> IRBuilder ()
incrClk val = do
  clk1 <- load regClk 0
  clk2 <- add clk1 $ literalAddr val
  store regClk 0 clk2

incrClkPageBoundaryReg :: Word16 -> Operand -> IRBuilder ()
incrClkPageBoundaryReg addr reg = do
  regVal <- load reg 0
  incrClkPageBoundary addr regVal

incrClkPageBoundary :: Word16 -> Operand -> IRBuilder ()
incrClkPageBoundary addr offset = do
  pageOff <- urem (literalAddr addr) $ literalAddr 0x100
  offExt <- zext offset i16
  newOff <- add pageOff offExt
  gt <- icmp P.UGE newOff $ literalAddr 0x100
  toAdd <- select gt (literalAddr 1) $ literalAddr 0
  clk1 <- load regClk 0
  clk2 <- add clk1 $ toAdd
  store regClk 0 clk2