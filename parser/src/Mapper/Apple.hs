{-# LANGUAGE BlockArguments, OverloadedStrings, RecursiveDo, TemplateHaskell #-}
module Mapper.Apple (
  appleMapper
) where

import Data.Bits (shift)
import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Data.Word (Word16)
import qualified LLVM.AST.IntegerPredicate as IntegerPredicate
import qualified LLVM.AST.Type as Type
import LLVM.AST.Operand (Operand)
import LLVM.IRBuilder
import Mapper.Base (Mapper(..))
import Mapper.Util (buildGlobal, buildMem, buildRom, literal, literalAddr, putChar, putCharDef, readRom)

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
          call Mapper.Util.putChar [(literal 0x0a, [])]
          writeCharCount $ literal 0
          retVoid
          elseOutputNewLine <- block `named` "elseOutputNewLine"
          call Mapper.Util.putChar [(charToOutput, [])]
          charCount <- readCharCount
          condNewLine <- icmp IntegerPredicate.UGE charCount $ literal 39
          condBr condNewLine ifNewLine elseNewLine
          ifNewLine <- block `named` "ifNewLine"
          call Mapper.Util.putChar [(literal 0x0a, [])]
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