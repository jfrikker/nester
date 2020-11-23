module Passes (
  functionBodies,
  passBase,
  smbSwitchPass
) where

import AddressSpace (AddressSpace)
import qualified Assembly as I
import Data.Functor ((<&>))
import Data.Word (Word16)

data Parser = Parser {
  readInstruction :: Word16 -> AddressSpace -> I.Instruction
}

type Pass = Parser -> Parser

functionBodies :: Parser -> [Word16] -> AddressSpace -> [(Word16, [I.Instruction])]
functionBodies p = I.functionBodiesWithParser $ readInstruction p

passBase :: Parser
passBase = Parser {
  readInstruction = I.readInstruction
}

smbSwitchPass :: Pass
smbSwitchPass underlying = Parser {
    readInstruction = readInstruction_
  }
  where readInstruction_ off = do
          inst <- readInstruction underlying off
          case inst of
            (I.Absolute _ I.JSR 0x8e04) -> do
              end <- I.readAddress $ off + 3
              addrs <- mapM I.readAddress [off + 3, (off + 5)..] <&> takeWhile (\o -> o >= 0x8000)
              return $ I.Switch off I.SWA 3 addrs
            otherwise -> return inst