{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecursiveDo #-}
module Mapper (
  Mapper(..),
  irqAddress,
  mapper0,
  nmiAddress,
  readStaticAddress,
  readRom,
  readStaticValue,
  resetAddress
) where

import Data.Bits (shiftL)
import qualified Data.ByteString as BS
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Data.Word (Word16, Word8)
import LLVM.AST (Definition(GlobalDefinition))
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
import LLVM.AST.Operand (Operand)
import LLVM.IRBuilder
import LLVM.Types (readCallbackType, writeCallbackType)

data Mapper = Mapper {
  readStatic :: Word16 -> Word8,
  globals :: [Definition],
  readBody :: Operand -> (Operand -> IRBuilder Operand) -> IRBuilder (),
  writeBody :: Operand -> Operand -> (Operand -> Operand -> IRBuilder ()) -> IRBuilder ()
}

readRom :: Word16 -> BS.ByteString -> Word16 -> Maybe Word8
readRom offset rom idx
  | idx < offset = Nothing
  | idx -  offset >= fromIntegral (BS.length rom) = Nothing
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

mapper0 :: BS.ByteString -> BS.ByteString -> Mapper
mapper0 prgRom chrRom = Mapper {
    readStatic = readStatic,
    globals = globals,
    readBody = readBody,
    writeBody = writeBody
  }
  where readStatic = fromJust . readRom 32768 prgRom
        prgRomDef = GlobalDefinition Global.globalVariableDefaults {
          Global.name = "prgRom",
          Global.type' = ArrayType 32768 i8,
          Global.linkage = Linkage.Private,
          Global.isConstant = True,
          Global.initializer = Just $ Constant.Array {
            Constant.memberType = i8,
            Constant.memberValues = map (Constant.Int 8 . fromIntegral) $ BS.unpack prgRom
          }
        }
        chrRomDef = GlobalDefinition Global.globalVariableDefaults {
          Global.name = "chrRom",
          Global.type' = ArrayType 8192 i8,
          Global.linkage = Linkage.Private,
          Global.isConstant = True,
          Global.initializer = Just $ Constant.Array {
            Constant.memberType = i8,
            Constant.memberValues = map (Constant.Int 8 . fromIntegral) $ BS.unpack chrRom
          }
        }
        memDef = GlobalDefinition Global.globalVariableDefaults {
          Global.name = "mem",
          Global.type' = ArrayType 32768 i8,
          Global.linkage = Linkage.Private,
          Global.initializer = Just $ Constant.Null $ ArrayType 32768 i8
        }
        mem = Operand.ConstantOperand $ Constant.GlobalReference (ptr $ ArrayType 32768 i8) "mem"
        prgRomRef = Operand.ConstantOperand $ Constant.GlobalReference (ptr $ ArrayType 32768 i8) "prgRom"
        getMemValue addr = do
          addr' <- emitInstr (ptr i8) $ Instruction.GetElementPtr True mem [Operand.ConstantOperand $ Constant.Int 32 0, addr] []
          load addr' 0
        getRomValue addr = do
          addr' <- emitInstr (ptr i8) $ Instruction.GetElementPtr True prgRomRef [Operand.ConstantOperand $ Constant.Int 32 0, addr] []
          load addr' 0
        readBody addr readCallback = mdo
          _entry <- block `named` "entry"
          condLow <- icmp IntegerPredicate.ULT addr $ literalAddr 0x2000
          condBr condLow ifLow elseLow
          ifLow <- block `named` "ifLow"
          ifLowLoc <- urem addr $ literalAddr 0x800
          ifLowVal <- getMemValue ifLowLoc
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
          memVal <- getMemValue addr
          ret memVal
          end <- block `named` "end"
          romAddr <- sub addr $ literalAddr 0x8000
          romVal <- getRomValue romAddr
          ret romVal
        setMemValue addr val = do
          addr' <- emitInstr (ptr i8) $ Instruction.GetElementPtr True mem [Operand.ConstantOperand $ Constant.Int 32 0, addr] []
          store addr' 0 val
        writeBody addr val writeCallback = mdo
          _entry <- block `named` "entry"
          condLow <- icmp IntegerPredicate.ULT addr $ literalAddr 0x2000
          condBr condLow ifLow elseLow
          ifLow <- block `named` "ifLow"
          ifLowLoc <- urem addr $ literalAddr 0x800
          setMemValue ifLowLoc val
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
          setMemValue addr val
          retVoid
          end <- block `named` "end"
          retVoid
        globals = [prgRomDef, chrRomDef, memDef]