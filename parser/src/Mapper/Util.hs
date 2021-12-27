{-# LANGUAGE BlockArguments, OverloadedStrings, RecursiveDo, TemplateHaskell #-}
module Mapper.Util (
  buildGlobal,
  buildMem,
  buildRom,
  literal,
  literalAddr,
  Mapper.Util.putChar,
  putCharDef,
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

buildMem :: Word64 -> Name -> (Definition, Operand -> IRBuilder Operand, Operand -> Operand -> IRBuilder ())
buildMem size name = (def, read, write)
  where def = GlobalDefinition Global.globalVariableDefaults {
          Global.name = name,
          Global.type' = Type.ArrayType size Type.i8,
          Global.linkage = Linkage.Private,
          Global.initializer = Just $ Constant.Null $ Type.ArrayType size Type.i8
        }
        mem = Operand.ConstantOperand $ Constant.GlobalReference (Type.ptr $ Type.ArrayType size Type.i8) name
        read addr = do
          addr' <- emitInstr (Type.ptr Type.i8) $ Instruction.GetElementPtr True mem [Operand.ConstantOperand $ Constant.Int 32 0, addr] []
          load addr' 0
        write  addr val = do
          addr' <- emitInstr (Type.ptr Type.i8) $ Instruction.GetElementPtr True mem [Operand.ConstantOperand $ Constant.Int 32 0, addr] []
          store addr' 0 val

buildRom :: Name -> BS.ByteString -> (Definition, Operand -> IRBuilder Operand)
buildRom name contents = (def, read)
  where def = GlobalDefinition Global.globalVariableDefaults {
          Global.name = name,
          Global.type' = Type.ArrayType size Type.i8,
          Global.linkage = Linkage.Private,
          Global.isConstant = True,
          Global.initializer = Just $ Constant.Array {
            Constant.memberType = Type.i8,
            Constant.memberValues = map (Constant.Int 8 . fromIntegral) $ BS.unpack contents
          }
        }
        rom = Operand.ConstantOperand $ Constant.GlobalReference (Type.ptr $ Type.ArrayType size Type.i8) name
        read addr = do
          addr' <- emitInstr (Type.ptr Type.i8) $ Instruction.GetElementPtr True rom [Operand.ConstantOperand $ Constant.Int 32 0, addr] []
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
        mem = Operand.ConstantOperand $ Constant.GlobalReference (Type.ptr type') name
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
putChar = Operand.ConstantOperand $ Constant.GlobalReference (Type.ptr $ Type.FunctionType {
  Type.resultType = Type.i32,
  Type.argumentTypes = [Type.i8],
  Type.isVarArg = False
}) "putchar"