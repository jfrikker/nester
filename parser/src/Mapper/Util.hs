{-# LANGUAGE BlockArguments, OverloadedStrings, RecursiveDo, TemplateHaskell #-}
module Mapper.Util (
  buildConstantAddr,
  buildRom,
  literal,
  literalAddr,
  readRom
) where

import qualified Data.ByteString as BS
import Data.Word (Word64, Word16, Word8)
import LLVM.AST (Definition(GlobalDefinition), Name)
import qualified LLVM.AST.Constant as Constant
import qualified LLVM.AST.FunctionAttribute as FunctionAttribute
import qualified LLVM.AST.Global as Global
import qualified LLVM.AST.Instruction as Instruction
import qualified LLVM.AST.Linkage as Linkage
import qualified LLVM.AST.Operand as Operand
import qualified LLVM.AST.Type as Type
import LLVM.AST.Operand (Operand)
import LLVM.IRBuilder

readRom :: Word16 -> BS.ByteString -> Word16 -> Maybe Word8
readRom offset rom idx
  | idx < offset = Nothing
  | idx - offset >= fromIntegral (BS.length rom) = Nothing
  | otherwise = Just $ BS.index rom (fromIntegral $ idx - offset)

literalAddr :: Word16 -> Operand
literalAddr = Operand.ConstantOperand . Constant.Int 16 . fromIntegral

literal :: Word8 -> Operand
literal = Operand.ConstantOperand . Constant.Int 8 . fromIntegral

buildRom :: Name -> BS.ByteString -> (Definition, Operand)
buildRom name contents = (def, rom)
  where def = GlobalDefinition Global.globalVariableDefaults {
          Global.name = name,
          Global.type' = Type.ArrayType size Type.i8,
          Global.isConstant = True,
          Global.initializer = Just $ Constant.Array {
            Constant.memberType = Type.i8,
            Constant.memberValues = map (Constant.Int 8 . fromIntegral) $ BS.unpack contents
          }
        }
        rom = Operand.ConstantOperand $ Constant.GlobalReference (Type.ptr $ Type.ArrayType size Type.i8) name
        size = fromIntegral $ BS.length contents

buildConstantAddr :: Name -> Word16 -> (Definition, Operand)
buildConstantAddr name contents = (def, symb)
  where def = GlobalDefinition Global.globalVariableDefaults {
          Global.name = name,
          Global.type' = Type.i16,
          Global.isConstant = True,
          Global.initializer = Just $ Constant.Int 16 $ fromIntegral contents
        }
        symb = Operand.ConstantOperand $ Constant.GlobalReference Type.i16 name