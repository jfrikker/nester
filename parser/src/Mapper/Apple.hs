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
import Mapper.Util (buildConstantAddr, buildRom, literal, literalAddr, readRom)

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
        (prgRomDef, readPrgRom) = buildRom "rom" rom
        (romOffset, _) = buildConstantAddr "romOffset" off
        (romSize, _) = buildConstantAddr "romSize" $ fromIntegral $ BS.length rom
        globals = [prgRomDef, romOffset, romSize]