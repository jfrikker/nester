{-# LANGUAGE BlockArguments, OverloadedStrings, RecursiveDo, TemplateHaskell #-}
module Mapper (
  Mapper(..),
  irqAddress,
  mapper0,
  appleMapper,
  nmiAddress,
  readStaticAddress,
  readRom,
  readStaticValue,
  resetAddress
) where

import Data.Bits (shiftL, shift)
import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile)
import Data.Foldable (asum)
import Data.Functor ((<&>))
import Data.Maybe (fromJust, fromMaybe)
import Data.Word (Word64, Word16, Word8)
import LLVM.AST (Definition(GlobalDefinition), Name)
import qualified LLVM.AST.CallingConvention as CallingConvention
import qualified LLVM.AST.Constant as Constant
import qualified LLVM.AST.FunctionAttribute as FunctionAttribute
import qualified LLVM.AST.Global as Global
import qualified LLVM.AST.Instruction as Instruction
import qualified LLVM.AST.IntegerPredicate as IntegerPredicate
import qualified LLVM.AST.Linkage as Linkage
import qualified LLVM.AST.Operand as Operand
import LLVM.AST.Type (Type(ArrayType, VoidType), i8, i16, ptr)
import qualified LLVM.AST.Type as Type
import LLVM.AST.Operand (Operand, DWOpFragment (offset))
import LLVM.IRBuilder
import LLVM.Types (callbackType)
import Text.ParserCombinators.ReadP (endBy)
import qualified Data.ByteString as Bs

data Mapper = Mapper {
  readStatic :: Word16 -> Word8,
  globals :: [Definition],
  readBody :: Operand -> (Operand -> IRBuilder Operand) -> IRBuilder (),
  writeBody :: Operand -> Operand -> (Operand -> Operand -> IRBuilder ()) -> IRBuilder (),
  mapperId :: Word8
}

readRom :: Word16 -> BS.ByteString -> Word16 -> Maybe Word8
readRom offset rom idx
  | idx < offset = Nothing
  | idx - offset >= fromIntegral (BS.length rom) = Nothing
  | otherwise = Just $ BS.index rom (fromIntegral $ idx - offset)

readStaticValue :: Word16 -> Mapper -> Word8
readStaticValue = flip readStatic

readStaticAddress :: Word16 -> Mapper -> Word16
readStaticAddress offset = do
  low <- readStaticValue offset <&> fromIntegral
  high <- readStaticValue (offset + 1) <&> fromIntegral
  return $ low + shiftL high 8

resetAddress :: Mapper -> Word16
resetAddress = readStaticAddress 0xfffc

irqAddress :: Mapper -> Word16
irqAddress = readStaticAddress 0xfffe

nmiAddress :: Mapper -> Word16
nmiAddress = readStaticAddress 0xfffa

literalAddr :: Word16 -> Operand
literalAddr = Operand.ConstantOperand . Constant.Int 16 . fromIntegral

literal :: Word8 -> Operand
literal = Operand.ConstantOperand . Constant.Int 8 . fromIntegral

buildMem :: Word64 -> Name -> (Definition, Operand -> IRBuilder Operand, Operand -> Operand -> IRBuilder ())
buildMem size name = (def, read, write)
  where def = GlobalDefinition Global.globalVariableDefaults {
          Global.name = name,
          Global.type' = ArrayType size i8,
          Global.linkage = Linkage.Private,
          Global.initializer = Just $ Constant.Null $ ArrayType size i8
        }
        mem = Operand.ConstantOperand $ Constant.GlobalReference (ptr $ ArrayType size i8) name
        read addr = do
          addr' <- emitInstr (ptr i8) $ Instruction.GetElementPtr True mem [Operand.ConstantOperand $ Constant.Int 32 0, addr] []
          load addr' 0
        write  addr val = do
          addr' <- emitInstr (ptr i8) $ Instruction.GetElementPtr True mem [Operand.ConstantOperand $ Constant.Int 32 0, addr] []
          store addr' 0 val

buildRom :: Name -> BS.ByteString -> (Definition, Operand -> IRBuilder Operand)
buildRom name contents = (def, read)
  where def = GlobalDefinition Global.globalVariableDefaults {
          Global.name = name,
          Global.type' = ArrayType size i8,
          Global.linkage = Linkage.Private,
          Global.isConstant = True,
          Global.initializer = Just $ Constant.Array {
            Constant.memberType = i8,
            Constant.memberValues = map (Constant.Int 8 . fromIntegral) $ BS.unpack contents
          }
        }
        rom = Operand.ConstantOperand $ Constant.GlobalReference (ptr $ ArrayType size i8) name
        read addr = do
          addr' <- emitInstr (ptr i8) $ Instruction.GetElementPtr True rom [Operand.ConstantOperand $ Constant.Int 32 0, addr] []
          load addr' 0
        size = fromIntegral $ BS.length contents

buildGlobal :: Name -> Type.Type -> (Definition, IRBuilder Operand, Operand -> IRBuilder ())
buildGlobal name type' = (def, read, write)
  where def = GlobalDefinition Global.globalVariableDefaults {
          Global.name = name,
          Global.type' = type',
          Global.linkage = Linkage.Private,
          Global.initializer = Just $ Constant.Int 8 0
        }
        mem = Operand.ConstantOperand $ Constant.GlobalReference (ptr type') name
        read = load mem 0
        write = store mem 0

putCharDef :: Definition
putCharDef = GlobalDefinition Global.functionDefaults {
  Global.name = "putchar",
  Global.returnType = Type.i32,
  Global.parameters = ([Global.Parameter Type.i8 "c" []], False),
  Global.functionAttributes = [Right FunctionAttribute.WriteOnly]
}

putChar :: Operand
putChar = Operand.ConstantOperand $ Constant.GlobalReference (ptr $ Type.FunctionType {
  Type.resultType = Type.i32,
  Type.argumentTypes = [Type.i8],
  Type.isVarArg = False
}) "putchar"

mapper0 :: BS.ByteString -> BS.ByteString -> Mapper
mapper0 prgRom chrRom = Mapper {
    readStatic = readStatic,
    globals = globals,
    readBody = readBody,
    writeBody = writeBody,
    mapperId = 0
  }
  where readStatic = fromJust . readRom 32768 prgRom
        (lowMemDef, readLowMem, writeLowMem) = buildMem 0x800 "lowMem"
        (cartMemDef, readCartMem, writeCartMem) = buildMem 0x2000 "cartMem"
        (prgRomDef, readPrgRom) = buildRom "prgRom" prgRom
        (chrRomDef, _) = buildRom "chrRom" chrRom
        readBody addr readCallback = mdo
          _entry <- block `named` "entry"
          condLow <- icmp IntegerPredicate.ULT addr $ literalAddr 0x2000
          condBr condLow ifLow elseLow
          ifLow <- block `named` "ifLow"
          ifLowLoc <- urem addr $ literalAddr 0x800
          ifLowVal <- readLowMem ifLowLoc
          ret ifLowVal
          elseLow <- block `named` "elseLow"
          condPpu <- icmp IntegerPredicate.ULT addr $ literalAddr 0x4000
          condBr condPpu ifPpu elsePpu
          ifPpu <- block `named` "ifPpu"
          ppuLocTemp <- sub addr $ literalAddr 0x2000
          ppuLocTemp2 <- urem ppuLocTemp $ literalAddr 0x100
          ppuLoc <- add ppuLocTemp2 $ literalAddr 0x2000
          ppuVal <- readCallback ppuLoc
          ret ppuVal
          elsePpu <- block `named` "elsePpu"
          condApu <- icmp IntegerPredicate.ULT addr $ literalAddr 0x4020
          condBr condApu ifApu elseApu
          ifApu <- block `named` "ifApu"
          apuVal <- readCallback addr
          ret apuVal
          elseApu <- block `named` "elseApu"
          condRam <- icmp IntegerPredicate.ULT addr $ literalAddr 0x8000
          condBr condRam ifRam end
          ifRam <- block `named` "ifRam"
          memVal <- readCartMem addr
          ret memVal
          end <- block `named` "end"
          romAddr <- sub addr $ literalAddr 0x8000
          romVal <- readPrgRom romAddr
          ret romVal
        writeBody addr val writeCallback = mdo
          _entry <- block `named` "entry"
          condLow <- icmp IntegerPredicate.ULT addr $ literalAddr 0x2000
          condBr condLow ifLow elseLow
          ifLow <- block `named` "ifLow"
          ifLowLoc <- urem addr $ literalAddr 0x800
          writeLowMem ifLowLoc val
          retVoid
          elseLow <- block `named` "elseLow"
          condPpu <- icmp IntegerPredicate.ULT addr $ literalAddr 0x4000
          condBr condPpu ifPpu elsePpu
          ifPpu <- block `named` "ifPpu"
          ppuLocTemp <- sub addr $ literalAddr 0x2000
          ppuLocTemp2 <- urem ppuLocTemp $ literalAddr 0x100
          ppuLoc <- add ppuLocTemp2 $ literalAddr 0x2000
          writeCallback ppuLoc val
          retVoid
          elsePpu <- block `named` "elsePpu"
          condApu <- icmp IntegerPredicate.ULT addr $ literalAddr 0x4020
          condBr condApu ifApu elseApu
          ifApu <- block `named` "ifApu"
          writeCallback addr val
          retVoid
          elseApu <- block `named` "elseApu"
          condRam <- icmp IntegerPredicate.ULT addr $ literalAddr 0x8000
          condBr condRam ifRam end
          ifRam <- block `named` "ifRam"
          writeCartMem addr val
          retVoid
          end <- block `named` "end"
          retVoid
        getChrRom = GlobalDefinition $ Global.functionDefaults {
            Global.name = "mapperId",
            Global.parameters = ([], False),
            Global.returnType = Type.ptr $ Type.ArrayType 0x2000 i8,
            Global.basicBlocks = execIRBuilder emptyIRBuilder $ ret (Operand.ConstantOperand $ Constant.GlobalReference (ptr $ ArrayType 0x2000 i8) "chrRom")
          }
        globals = [prgRomDef, chrRomDef, lowMemDef, cartMemDef, getChrRom]

appleMon :: BS.ByteString 
appleMon = $(embedFile "appleMon.bin")

inRange :: Word16 -> Word16 -> Operand -> IRBuilder Operand
inRange low high addr = do
    condLow <- icmp IntegerPredicate.UGE addr $ literalAddr low
    condHigh <- icmp IntegerPredicate.ULE addr $ literalAddr high
    LLVM.IRBuilder.and condLow condHigh

appleMapper :: Word16 -> BS.ByteString -> Mapper
appleMapper off rom = Mapper {
    readStatic = readStatic,
    readBody = readBody,
    writeBody = writeBody,
    globals = globals,
    mapperId = 0
  }
  where readStatic 0xfffe = fromIntegral off
        readStatic 0xffff = fromIntegral $ shift off (-8)
        readStatic 0xfffc = fromIntegral off
        readStatic 0xfffd = fromIntegral $ shift off (-8)
        readStatic 0xfffa = fromIntegral off
        readStatic 0xfffb = fromIntegral $ shift off (-8)
        readStatic memOff = fromMaybe 0 $ asum [readRom off rom memOff, readRom 0xff00 appleMon memOff]
        (memDef, readMem, writeMem) = buildMem 0x10000 "mem"
        (prgRomDef, readPrgRom) = buildRom "rom" rom
        (charCountDef, readCharCount, writeCharCount) = buildGlobal "charCount" Type.i8
        readBody addr readCallback = mdo
          _entry <- block `named` "entry"
          condIO <- inRange 0xD010 0xD013 addr
          condBr condIO ifIO elseIO
          ifIO <- block `named` "ifIO"
          ioVal <- readCallback addr
          ret ioVal
          elseIO <- block `named` "elseIO"
          condRom <- inRange off (off + fromIntegral (BS.length rom)) addr
          condBr condRom ifRom elseRom
          ifRom <- block `named` "ifRom"
          romLoc <- sub addr $ literalAddr off
          romVal <- readPrgRom romLoc
          ret romVal
          elseRom <- block `named` "elseRom"
          elseVal <- readMem addr
          ret elseVal
        writeToScreen val = mdo
          charToOutput <- LLVM.IRBuilder.and val $ literal 0x7f
          condOutputNewLine <- icmp IntegerPredicate.EQ charToOutput $ literal 0x0d
          condBr condOutputNewLine ifOutputNewLine elseOutputNewLine
          ifOutputNewLine <- block `named` "ifOutputNewLine"
          call Mapper.putChar [(literal 0x0a, [])]
          writeCharCount $ literal 0
          retVoid
          elseOutputNewLine <- block `named` "elseOutputNewLine"
          call Mapper.putChar [(charToOutput, [])]
          charCount <- readCharCount
          condNewLine <- icmp IntegerPredicate.UGE charCount $ literal 39
          condBr condNewLine ifNewLine elseNewLine
          ifNewLine <- block `named` "ifNewLine"
          call Mapper.putChar [(literal 0x0a, [])]
          writeCharCount $ literal 0
          retVoid
          elseNewLine <- block `named` "elseNewLine"
          newCharCount <- add charCount $ literal 1
          writeCharCount newCharCount
          retVoid
        writeBody addr val writeCallback = mdo
          _entry <- block `named` "entry"
          condDspData <- icmp IntegerPredicate.EQ addr $ literalAddr 0xD012
          condBr condDspData ifDspData elseDspData
          ifDspData <- block `named` "ifDspData"
          writeToScreen val
          elseDspData <- block `named` "elseDspData"
          condIO <- inRange 0xD010 0xD013 addr
          condBr condIO ifIO elseIO
          ifIO <- block `named` "ifIO"
          writeCallback addr val
          retVoid
          elseIO <- block `named` "elseIO"
          condRom <- inRange off (off + fromIntegral (BS.length rom)) addr
          condBr condRom ifRom elseRom
          ifRom <- block `named` "ifRom"
          retVoid
          elseRom <- block `named` "elseRom"
          writeMem addr val
          retVoid
        globals = [putCharDef, prgRomDef, memDef, charCountDef]