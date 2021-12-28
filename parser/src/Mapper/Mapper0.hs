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
import qualified LLVM.AST.Linkage as Linkage
import qualified LLVM.AST.Operand as Operand
import LLVM.AST.Operand (Operand)
import qualified LLVM.AST.Type as Type
import LLVM.IRBuilder
import Mapper.Base (Mapper(..))
import Mapper.Util (buildRom, literal, literalAddr, readRom)

mapper0 :: BS.ByteString -> BS.ByteString -> Mapper
mapper0 prgRom chrRom = Mapper {
    readStatic = readStatic,
    globals = globals,
    mapperId = 0
  }
  where readStatic = fromJust . readRom 32768 prgRom
        (prgRomDef, readPrgRom) = buildRom "prgRom" prgRom
        (chrRomDef, _) = buildRom "chrRom" chrRom
        globals = [chrRomDef, prgRomDef]