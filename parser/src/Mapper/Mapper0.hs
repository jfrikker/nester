{-# LANGUAGE OverloadedStrings, RecursiveDo #-}
module Mapper.Mapper0(
  mapper0
) where

import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import LLVM.AST (Definition(GlobalDefinition))
import qualified LLVM.AST.Constant as Constant
import qualified LLVM.AST.Global as Global
import qualified LLVM.AST.IntegerPredicate as IntegerPredicate
import qualified LLVM.AST.Operand as Operand
import qualified LLVM.AST.Type as Type
import LLVM.IRBuilder
import Mapper.Base (Mapper(..))
import Mapper.Util (buildMem, buildRom, literalAddr, readRom)

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
            Global.returnType = Type.ptr $ Type.ArrayType 0x2000 Type.i8,
            Global.basicBlocks = execIRBuilder emptyIRBuilder $ ret (Operand.ConstantOperand $ Constant.GlobalReference (Type.ptr $ Type.ArrayType 0x2000 Type.i8) "chrRom")
          }
        globals = [prgRomDef, chrRomDef, lowMemDef, cartMemDef, getChrRom]