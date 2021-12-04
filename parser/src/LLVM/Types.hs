module LLVM.Types (
  callbackType
) where
import qualified LLVM.AST.Type as Type

callbackType :: Type.Type
callbackType = Type.FunctionType {
  Type.resultType = Type.i8,
  Type.argumentTypes = [Type.i8, Type.i16, Type.i8],
  Type.isVarArg = False
}