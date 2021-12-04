module LLVM.Types (
  readCallbackType,
  writeCallbackType
) where
import qualified LLVM.AST.Type as Type

readCallbackType :: Type.Type
readCallbackType = Type.FunctionType {
  Type.resultType = Type.i8,
  Type.argumentTypes = [Type.i16, Type.i16],
  Type.isVarArg = False
}

writeCallbackType :: Type.Type
writeCallbackType = Type.FunctionType {
  Type.resultType = Type.VoidType,
  Type.argumentTypes = [Type.i16, Type.i8, Type.i16],
  Type.isVarArg = False
}