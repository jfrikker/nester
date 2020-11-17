{-# LANGUAGE OverloadedStrings, RecursiveDo #-}
module LLVM (
  toIRNes
) where

import AddressSpace(AddressSpace)
import LLVM.AST hiding (function)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.IntegerPredicate as P
import qualified LLVM.AST.Linkage as L
import LLVM.AST.Type (i8, i16, ptr)
import LLVM.IRBuilder

nesMem :: Definition
nesMem = GlobalDefinition $ globalVariableDefaults {
  G.name = "mem",
  G.type' = ArrayType 65536 i8,
  G.linkage = L.Private,
  G.initializer = Just $ C.Null $ ArrayType 65536 i8
  }

nesMemRef :: Operand
nesMemRef = ConstantOperand $ C.GlobalReference (ptr $ ArrayType 65536 i8) "mem"

initNesModule :: ModuleBuilder ()
initNesModule = do
  emitDefn nesMem
  function "readMem" [(i16, "addr")] i8 $ \[addr] -> mdo
    _entry <- block `named` "entry"
    condZero <- icmp P.ULT addr $ ConstantOperand $ C.Int 16 0xff
    condBr condZero ifZero end
    ifZero <- block `named` "ifZero"
    ret $ ConstantOperand $ C.Int 8 0
    end <- block `named` "end"
    memAddr <- gep nesMemRef [ConstantOperand $ C.Int 16 0xff, addr]
    val <- load memAddr 0
    ret val
  return ()

toIRNes :: AddressSpace -> Module
toIRNes mem = buildModule "nes" $ do
  initNesModule