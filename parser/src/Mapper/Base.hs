{-# LANGUAGE BlockArguments, OverloadedStrings, RecursiveDo, TemplateHaskell #-}
module Mapper.Base (
  Mapper(..),
  irqAddress,
  nmiAddress,
  readStaticAddress,
  readStaticValue,
  resetAddress
) where

import Data.Bits (shiftL)
import Data.Functor ((<&>))
import Data.Word (Word16, Word8)
import LLVM.AST (Definition)
import LLVM.AST.Operand (Operand)
import LLVM.IRBuilder

data Mapper = Mapper {
  readStatic :: Word16 -> Word8,
  globals :: [Definition],
  readBody :: Operand -> (Operand -> IRBuilder Operand) -> IRBuilder (),
  writeBody :: Operand -> Operand -> (Operand -> Operand -> IRBuilder ()) -> IRBuilder (),
  mapperId :: Word8
}

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