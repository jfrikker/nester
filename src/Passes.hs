module Passes (
  functionBodies,
  passBase,
  smbSwitchPass
) where

import AddressSpace (AddressSpace)
import qualified Assembly as I
import Data.Functor ((<&>))
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Word (Word16)

data Parser = Parser {
  postProcess :: [Word16] -> Map Word16 (Map Word16 I.Instruction) -> Map Word16 (Map Word16 I.Instruction),
  readInstruction :: Word16 -> AddressSpace -> I.Instruction
}

type Pass = Parser -> Parser

functionBodies :: Parser -> [Word16] -> AddressSpace -> Map Word16 (Map Word16 I.Instruction)
functionBodies p offs addr = postProcess p offs funcs
  where funcs = I.functionBodiesWithParser (\off -> readInstruction p off addr) offs

passBase :: Parser
passBase = Parser {
  postProcess = \offs -> id,
  readInstruction = I.readInstruction
}

smbSwitchPass :: Pass
smbSwitchPass underlying = Parser {
    postProcess = postProcess_,
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
        postProcess_ offs funcs = postProcess underlying offs $ I.functionBodiesWithParser parse offs
          where filterAddrs (I.Switch off I.SWA len addrs) = I.Switch off I.SWA len $ filter isNotCode addrs
                filterAddrs i = i
                isNotCode a = null $ instructionsMatching (\i -> a == I.offset i) funcs
                repaired = mapInstructions filterAddrs $ postProcess underlying offs funcs
                flattened = foldr Map.union Map.empty $ Map.elems repaired
                parse = (!) flattened

mapInstructions :: (I.Instruction -> I.Instruction) -> Map Word16 (Map Word16 I.Instruction) -> Map Word16 (Map Word16 I.Instruction)
mapInstructions f = Map.map (Map.map f)

instructionsMatching :: (I.Instruction -> Bool) -> Map Word16 (Map Word16 I.Instruction) -> [I.Instruction]
instructionsMatching f = filter f . instructions

instructions :: Map Word16 (Map Word16 I.Instruction) -> [I.Instruction]
instructions funcs = do
  body <- Map.elems funcs
  Map.elems body