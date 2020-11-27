{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecursiveDo #-}
module LLVM (
  toIRNes
) where

import AddressSpace(AddressSpace, nmiAddress, resetAddress)
import qualified Assembly as I
import Control.Monad (forM_, void)
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
import LLVM.AST.Type (i1, i8, i16, ptr)
import LLVM.IRBuilder
import PyF (fmt)

instFunctionType :: Type
instFunctionType = FunctionType {
  resultType = VoidType,
  argumentTypes = [ptr callbacksStruct],
  isVarArg = False
}


readCallbackType :: Type
readCallbackType = FunctionType {
  resultType = i8,
  argumentTypes = [i16],
  isVarArg = False
}

writeCallbackType :: Type
writeCallbackType = FunctionType {
  resultType = VoidType,
  argumentTypes = [i16, i8],
  isVarArg = False
}

callbacksStructDef :: Definition
callbacksStructDef = TypeDefinition "struct.callbacks" $ Just StructureType {
    isPacked = False,
    elementTypes = [
      ptr readCallbackType,
      ptr writeCallbackType
    ]
  }

callbacksStruct :: Type
callbacksStruct = NamedTypeReference "struct.callbacks"

getReadCallback :: Operand -> IRBuilder Operand
getReadCallback cb = do
  addr <- emitInstr (ptr $ ptr readCallbackType) $ GetElementPtr True cb [ConstantOperand $ C.Int 32 0,
            ConstantOperand $ C.Int 32 0] []
  load addr 0

getWriteCallback :: Operand -> IRBuilder Operand
getWriteCallback cb = do
  addr <- emitInstr (ptr $ ptr writeCallbackType) $ GetElementPtr True cb [ConstantOperand $ C.Int 32 0,
            ConstantOperand $ C.Int 32 1] []
  load addr 0

memDef :: Definition
memDef = GlobalDefinition globalVariableDefaults {
  G.name = "mem",
  G.type' = ArrayType 65536 i8,
  G.linkage = L.Private,
  G.initializer = Just $ C.Null $ ArrayType 65536 i8
}

mem :: Operand
mem = ConstantOperand $ C.GlobalReference (ptr $ ArrayType 65536 i8) "mem"

getMemValue :: Operand -> IRBuilder Operand
getMemValue addr = do
  addr' <- emitInstr (ptr i8) $ GetElementPtr True mem [ConstantOperand $ C.Int 32 0, addr] []
  load addr' 0

setMemValue :: Operand -> Operand -> IRBuilder ()
setMemValue addr val = do
  addr' <- emitInstr (ptr i8) $ GetElementPtr True mem [ConstantOperand $ C.Int 32 0, addr] []
  store addr' 0 val

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

subCarryDef :: Definition
subCarryDef = GlobalDefinition functionDefaults {
  G.name = "llvm.usub.with.overflow.i8",
  G.returnType = StructureType {
    isPacked = False,
    elementTypes = [i8, i1]
  },
  G.parameters = ([Parameter i8 "a" [], Parameter i8 "b" []], False)
}

subCarry :: Operand -> Operand -> IRBuilder (Operand, Operand)
subCarry arg1 arg2 = do
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
  } "llvm.usub.with.overflow.i8"

nesReadMemDef :: Definition
nesReadMemDef = GlobalDefinition $ functionDefaults {
  G.name = "readMem",
  G.parameters = ([Parameter (ptr callbacksStruct) "callbacks" [], Parameter i16 "addr" []], False),
  G.returnType = i8,
  G.linkage = L.Private,
  G.basicBlocks = body
  }
  where callbacks = LocalReference (ptr callbacksStruct) "callbacks"
        addr = LocalReference i16 "addr"
        body = execIRBuilder emptyIRBuilder $ mdo
          _entry <- block `named` "entry"
          condLow <- icmp P.ULT addr $ literalAddr 0x2000
          condBr condLow ifLow elseLow
          ifLow <- block `named` "ifLow"
          ifLowLoc <- urem addr $ literalAddr 0x800
          ifLowVal <- getMemValue ifLowLoc
          ret ifLowVal
          elseLow <- block `named` "elseLow"
          condPpu <- icmp P.ULT addr $ literalAddr 0x4000
          condBr condPpu ifPpu elsePpu
          ifPpu <- block `named` "ifPpu"
          ppuLocTemp <- sub addr $ literalAddr 0x2000
          ppuLocTemp2 <- urem ppuLocTemp $ literalAddr 0x100
          ppuLoc <- add ppuLocTemp2 $ literalAddr 0x2000
          ppuVal <- doReadCallback ppuLoc
          ret ppuVal
          elsePpu <- block `named` "elsePpu"
          condApu <- icmp P.ULT addr $ literalAddr 0x4020
          condBr condApu ifApu end
          ifApu <- block `named` "ifApu"
          apuVal <- doReadCallback addr
          ret apuVal
          end <- block `named` "end"
          memVal <- getMemValue addr
          ret memVal
        doReadCallback addr = do
          readCb <- getReadCallback callbacks
          emitInstr i8 $ Call {
            tailCallKind = Nothing,
            callingConvention = CC.C,
            returnAttributes = [],
            LI.function = Right readCb,
            arguments = [(addr, [])],
            functionAttributes = [],
            metadata = []
          }

readMem :: Operand -> Operand -> IRBuilder Operand
readMem callbacks addr = call func [(callbacks, []), (addr, [])]
  where func = ConstantOperand $ C.GlobalReference FunctionType {
    resultType = i8,
    argumentTypes = [ptr callbacksStruct, i16],
    isVarArg = False
  } "readMem"

nesWriteMemDef :: Definition
nesWriteMemDef = GlobalDefinition $ functionDefaults {
  G.name = "writeMem",
  G.parameters = ([Parameter (ptr callbacksStruct) "callbacks" [], Parameter i16 "addr" [],
                   Parameter i8 "val" []], False),
  G.returnType = VoidType,
  G.linkage = L.Private,
  G.basicBlocks = body
  }
  where callbacks = LocalReference (ptr callbacksStruct) "callbacks"
        addr = LocalReference i16 "addr"
        val = LocalReference i8 "val"
        body = execIRBuilder emptyIRBuilder $ mdo
          _entry <- block `named` "entry"
          condLow <- icmp P.ULT addr $ literalAddr 0x2000
          condBr condLow ifLow elseLow
          ifLow <- block `named` "ifLow"
          ifLowLoc <- urem addr $ literalAddr 0x800
          setMemValue ifLowLoc val
          retVoid
          elseLow <- block `named` "elseLow"
          condPpu <- icmp P.ULT addr $ literalAddr 0x4000
          condBr condPpu ifPpu elsePpu
          ifPpu <- block `named` "ifPpu"
          ppuLocTemp <- sub addr $ literalAddr 0x2000
          ppuLocTemp2 <- urem ppuLocTemp $ literalAddr 0x100
          ppuLoc <- add ppuLocTemp2 $ literalAddr 0x2000
          doWriteCallback ppuLoc
          retVoid
          elsePpu <- block `named` "elsePpu"
          condApu <- icmp P.ULT addr $ literalAddr 0x4020
          condBr condApu ifApu end
          ifApu <- block `named` "ifApu"
          doWriteCallback addr
          retVoid
          end <- block `named` "end"
          setMemValue addr val
          retVoid
        doWriteCallback addr = do
          writeCb <- getWriteCallback callbacks
          emitInstrVoid $ Call {
            tailCallKind = Nothing,
            callingConvention = CC.C,
            returnAttributes = [],
            LI.function = Right writeCb,
            arguments = [(addr, []), (val, [])],
            functionAttributes = [],
            metadata = []
          }


writeMem :: Operand -> Operand -> Operand -> IRBuilder ()
writeMem state addr val = void $ call func [(state, []), (addr, []), (val, [])]
  where func = ConstantOperand $ C.GlobalReference FunctionType {
    resultType = VoidType,
    argumentTypes = [ptr callbacksStruct, i16, i8],
    isVarArg = False
  } "writeMem"

resetDef :: AddressSpace -> Definition
resetDef mem = GlobalDefinition $ functionDefaults {
  G.name = "reset",
  G.parameters = ([Parameter (ptr callbacksStruct) "callbacks" []], False),
  G.returnType = VoidType,
  G.basicBlocks = body
  }
  where cb = LocalReference (ptr callbacksStruct) "callbacks"
        body = execIRBuilder emptyIRBuilder $ do
          a <- alloca i8 Nothing 0
          x <- alloca i8 Nothing 0
          y <- alloca i8 Nothing 0
          n <- alloca i1 Nothing 0
          z <- alloca i1 Nothing 0
          v <- alloca i1 Nothing 0
          c <- alloca i1 Nothing 0
          s <- alloca i8 Nothing 0
          call (functionAtAddr $ resetAddress mem) [(cb, []), (a, []), (x, []), (y, []), (n, []), (z, []), (v, []),
            (c, []), (s, [])]
          retVoid

nmiDef :: AddressSpace -> Definition
nmiDef mem = GlobalDefinition $ functionDefaults {
  G.name = "nmi",
  G.parameters = ([Parameter (ptr callbacksStruct) "callbacks" []], False),
  G.returnType = VoidType,
  G.basicBlocks = body
  }
  where cb = LocalReference (ptr callbacksStruct) "callbacks"
        body = execIRBuilder emptyIRBuilder $ do
          a <- alloca i8 Nothing 0
          x <- alloca i8 Nothing 0
          y <- alloca i8 Nothing 0
          n <- alloca i1 Nothing 0
          z <- alloca i1 Nothing 0
          v <- alloca i1 Nothing 0
          c <- alloca i1 Nothing 0
          s <- alloca i8 Nothing 0
          call (functionAtAddr $ nmiAddress mem) [(cb, []), (a, []), (x, []), (y, []), (n, []), (z, []), (v, []),
            (c, []), (s, [])]
          retVoid

nesModuleDefs :: [Definition]
nesModuleDefs = [nesReadMemDef, nesWriteMemDef]

moduleDefs :: [Definition]
moduleDefs = [callbacksStructDef, memDef, subCarryDef]

toIRNes :: Map Word16 (Map Word16 I.Instruction) -> AddressSpace -> Module
toIRNes funcs mem = defaultModule {
  moduleName = "nes",
  moduleDefinitions = moduleDefs ++ nesModuleDefs ++ [resetDef mem, nmiDef mem] ++ irFuncs
  }
  where irFuncs = map (\(off, body) -> toIRFunction off $ Map.elems body) $ Map.assocs funcs

toIRFunction :: Word16 -> [I.Instruction] -> Definition
toIRFunction addr insts = GlobalDefinition $ functionDefaults {
  G.name = [fmt|func_{addr:04x}|],
  G.parameters = ([Parameter (ptr callbacksStruct) "callbacks" [],
                   Parameter (ptr i8) "regA" [],
                   Parameter (ptr i8) "regX" [], Parameter (ptr i8) "regY" [],
                   Parameter (ptr i1) "regN" [], Parameter (ptr i1) "regZ" [],
                   Parameter (ptr i1) "regV" [], Parameter (ptr i1) "regC" [],
                   Parameter (ptr i8) "regS" []], False),
  G.returnType = VoidType,
  G.linkage = L.Private,
  G.basicBlocks = body,
  G.functionAttributes = [Right FA.AlwaysInline]
  }
  where callbacks = LocalReference (ptr callbacksStruct) "callbacks"
        body = execIRBuilder emptyIRBuilder $ mdo
          block `named` "entry"
          localCb <- alloca callbacksStruct Nothing 0
          readSrc <- emitInstr (ptr $ ptr readCallbackType) $ GetElementPtr True callbacks [ConstantOperand $ C.Int 32 0,
                    ConstantOperand $ C.Int 32 0] []
          readTgt <- emitInstr (ptr $ ptr readCallbackType) $ GetElementPtr True localCb [ConstantOperand $ C.Int 32 0,
                    ConstantOperand $ C.Int 32 0] []
          read <- load readSrc 0
          store readTgt 0 read
          writeSrc <- emitInstr (ptr $ ptr writeCallbackType) $ GetElementPtr True callbacks [ConstantOperand $ C.Int 32 0,
                    ConstantOperand $ C.Int 32 1] []
          writeTgt <- emitInstr (ptr $ ptr writeCallbackType) $ GetElementPtr True localCb [ConstantOperand $ C.Int 32 0,
                    ConstantOperand $ C.Int 32 1] []
          write <- load writeSrc 0
          store writeTgt 0 write
          br [fmt|lbl_{addr:04x}_0|]
          forM_ insts $ toIR localCb

toIR :: Operand -> I.Instruction -> IRBuilder ()
toIR cb inst = do
  let off = I.offset inst
  block `named` [fmt|lbl_{off:04x}|]
  toIR_ cb inst

toIR_ :: Operand -> I.Instruction -> IRBuilder ()
toIR_ cb inst@(I.Absolute _ I.BIT arg) = do
  val <- readMem cb (literalAddr arg)
  nval <- lshr val $ literal 7
  nvalTrunc <- trunc nval i1
  store regN 0 nvalTrunc
  vval <- lshr val $ literal 6
  vvalTrunc <- trunc vval i1
  store regV 0 vvalTrunc
  a <- load regA 0
  res <- LLVM.IRBuilder.and a val
  setZ res
  brNext inst
toIR_ cb inst@(I.Absolute _ I.INC arg) = do
  val <- readMem cb (literalAddr arg)
  res <- add val $ literal 1
  writeMem cb (literalAddr arg) res
  setNZ res
  brNext inst
toIR_ _ (I.Absolute _ I.JMP arg) = do
  br [fmt|lbl_{arg:04x}_0|]
toIR_ cb inst@(I.Absolute _ I.JSR arg) = do
  call (functionAtAddr arg) [(cb, []), (regA, []), (regX, []),
    (regY, []), (regN, []), (regZ, []), (regV, []), (regC, []), (regS, [])]
  brNext inst
toIR_ cb inst@(I.Absolute _ I.LDA arg) = do
  val <- readMem cb (literalAddr arg)
  store regA 0 val
  setNZ val
  brNext inst
toIR_ cb inst@(I.Absolute _ I.STA arg) = do
  val <- load regA 0
  writeMem cb (literalAddr arg) val
  brNext inst
toIR_ cb inst@(I.AbsoluteX _ I.LDA arg) = do
  x <- load regX 0
  xExt <- zext x i16
  addr <- add xExt $ literalAddr arg
  val <- readMem cb addr
  store regA 0 val
  setNZ val
  brNext inst
toIR_ cb inst@(I.AbsoluteY _ I.LDA arg) = do
  y <- load regY 0
  yExt <- zext y i16
  addr <- add yExt $ literalAddr arg
  val <- readMem cb addr
  store regA 0 val
  setNZ val
  brNext inst
toIR_ cb inst@(I.AbsoluteY _ I.STA arg) = do
  val <- load regA 0
  y <- load regY 0
  yExt <- zext y i16
  addr <- add yExt $ literalAddr arg
  writeMem cb addr val
  brNext inst
toIR_ _ inst@(I.Immediate _ I.AND arg) = do
  a <- load regA 0
  newA <- LLVM.IRBuilder.and a $ literal arg
  store regA 0 newA
  setNZ newA
  brNext inst
toIR_ _ inst@(I.Immediate _ I.CMP arg) = do
  a <- load regA 0
  (res, carry) <- subCarry a $ literal arg
  setNZ res
  store regC 0 carry
  brNext inst
toIR_ _ inst@(I.Immediate _ I.LDA arg) = store regA 0 (literal arg) >> setNZ (literal arg) >> brNext inst
toIR_ _ inst@(I.Immediate _ I.LDX arg) = store regX 0 (literal arg) >> setNZ (literal arg) >> brNext inst
toIR_ _ inst@(I.Immediate _ I.LDY arg) = store regY 0 (literal arg) >> setNZ (literal arg) >> brNext inst
toIR_ _ inst@(I.Immediate _ I.ORA arg) = do
  a <- load regA 0
  newA <- LLVM.IRBuilder.or a $ literal arg
  store regA 0 newA
  setNZ newA
  brNext inst
toIR_ _ inst@(I.Implied _ I.DEX) = do
  x <- load regX 0
  newX <- sub x $ literal 1
  store regX 0 newX
  setNZ newX
  brNext inst
toIR_ _ inst@(I.Implied _ I.DEY) = do
  y <- load regY 0
  newY <- sub y $ literal 1
  store regY 0 newY
  setNZ newY
  brNext inst
toIR_ _ inst@(I.Implied _ I.INY) = do
  y <- load regY 0
  newY <- add y $ literal 1
  store regY 0 newY
  setNZ newY
  brNext inst
toIR_ _ inst@(I.Implied _ I.TAY) = do
  a <- load regA 0
  store regY 0 a
  setNZ a
  brNext inst
toIR_ _ inst@(I.Implied _ I.TXA) = do
  x <- load regX 0
  store regA 0 x
  setNZ x
  brNext inst
toIR_ _ inst@(I.Implied _ I.TXS) = do
  x <- load regX 0
  store regS 0 x
  brNext inst
toIR_ _ (I.Implied _ I.RTS) = retVoid
toIR_ _ inst@(I.Relative _ I.BCS _) = do
  cond <- load regC 0
  let next = I.nextAddr inst
  let branch = I.followingAddrs inst !! 1
  condBr cond [fmt|lbl_{branch:04x}_0|] [fmt|lbl_{next:04x}_0|] 
toIR_ _ inst@(I.Relative _ I.BEQ _) = do
  cond <- load regZ 0
  let next = I.nextAddr inst
  let branch = I.followingAddrs inst !! 1
  condBr cond [fmt|lbl_{branch:04x}_0|] [fmt|lbl_{next:04x}_0|]
toIR_ _ inst@(I.Relative _ I.BNE _) = do
  cond <- load regZ 0
  let next = I.nextAddr inst
  let branch = I.followingAddrs inst !! 1
  condBr cond [fmt|lbl_{next:04x}_0|] [fmt|lbl_{branch:04x}_0|]
toIR_ _ inst@(I.Relative _ I.BPL _) = do
  cond <- load regN 0
  let next = I.nextAddr inst
  let branch = I.followingAddrs inst !! 1
  condBr cond [fmt|lbl_{next:04x}_0|] [fmt|lbl_{branch:04x}_0|] 
toIR_ _ (I.Switch _ I.SWA _ tgts) = do
  cond <- load regA 0
  let first = head tgts
  switch cond [fmt|lbl_{first:04x}_0|] $ zipWith (curry (\(i, t) -> (C.Int 8 i, [fmt|lbl_{t:04x}_0|]))) [0..] tgts
toIR_ cb inst@(I.Zeropage _ I.LDA arg) = do
  addr <- zext (literal arg) i16
  val <- readMem cb addr
  store regA 0 val
  setNZ val
  brNext inst
toIR_ _ I.Unknown {} = retVoid -- TODO
toIR_ _ inst = brNext inst

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