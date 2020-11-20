{-# LANGUAGE DataKinds, QuasiQuotes, ScopedTypeVariables, TypeFamilies #-}

module Assembly (
  Instruction(..),
  Opcode(..),
  functionBody,
  localBranch,
  nextAddr,
  offset,
  readInstruction,
  readInstructions,
  toAssembly
) where

import AddressSpace (AddressSpace(readStatic), readAddress)
import Data.Bytes.Signed (signed)
import Data.Int(Int8)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word(Word8, Word16)
import Data.Maybe (catMaybes)
import Numeric (showHex)
import PyF (fmt)
import PyF.Class

data Opcode = ADC |
  AND |
  ASL |
  BCC |
  BCS |
  BEQ |
  BIT |
  BMI |
  BNE |
  BPL |
  BRK |
  BVC |
  BVS |
  CLC |
  CLD |
  CLI |
  CLV |
  CMP |
  CPX |
  CPY |
  DEC |
  DEX |
  DEY |
  EOR |
  INC |
  INX |
  INY |
  JMP |
  JSR |
  LDA |
  LDX |
  LDY |
  LSR |
  NOP |
  ORA |
  PHA |
  PHP |
  PLA |
  PLP |
  ROL |
  ROR |
  RTI |
  RTS |
  SBC |
  SEC |
  SED |
  SEI |
  STA |
  STX |
  STY |
  TAX |
  TAY |
  TSX |
  TXA |
  TXS |
  TYA
  deriving (Show)

instance PyFToString Opcode

type instance PyFClassify Opcode = 'PyFString

data Instruction = 
  Accumulator Word16 Opcode |
  Absolute Word16 Opcode Word16 |
  AbsoluteX Word16 Opcode Word16 |
  AbsoluteY Word16 Opcode Word16 |
  Immediate Word16 Opcode Word8 |
  Implied Word16 Opcode |
  Indirect Word16 Opcode Word16 |
  IndirectX Word16 Opcode Word8 |
  IndirectY Word16 Opcode Word8 |
  Relative Word16 Opcode Int8 |
  Zeropage Word16 Opcode Word8 |
  ZeropageX Word16 Opcode Word8 |
  ZeropageY Word16 Opcode Word8 |
  Unknown Word16
  deriving (Show)

offset :: Instruction -> Word16
offset (Accumulator off _) = off
offset (Absolute off _ _) = off
offset (AbsoluteX off _ _) = off
offset (AbsoluteY off _ _) = off
offset (Immediate off _ _) = off
offset (Implied off _) = off
offset (Indirect off _ _) = off
offset (IndirectX off _ _) = off
offset (IndirectY off _ _) = off
offset (Relative off _ _) = off
offset (Zeropage off _ _) = off
offset (ZeropageX off _ _) = off
offset (ZeropageY off _ _) = off
offset (Unknown off) = off

opcode :: Instruction -> Opcode
opcode (Accumulator _ op) = op
opcode (Absolute _ op _) = op
opcode (AbsoluteX _ op _) = op
opcode (AbsoluteY _ op _) = op
opcode (Immediate _ op _) = op
opcode (Implied _ op) = op
opcode (Indirect _ op _) = op
opcode (IndirectX _ op _) = op
opcode (IndirectY _ op _) = op
opcode (Relative _ op _) = op
opcode (Zeropage _ op _) = op
opcode (ZeropageX _ op _) = op
opcode (ZeropageY _ op _) = op

binLength :: Instruction -> Word16
binLength (Accumulator _ _) = 1
binLength (Absolute _ _ _) = 3
binLength (AbsoluteX _ _ _) = 3
binLength (AbsoluteY _ _ _) = 3
binLength (Immediate _ _ _) = 2
binLength (Implied _ _) = 1
binLength (Indirect _ _ _) = 3
binLength (IndirectX _ _ _) = 2
binLength (IndirectY _ _ _) = 2
binLength (Relative _ _ _) = 2
binLength (Zeropage _ _ _) = 2
binLength (ZeropageX _ _ _) = 2
binLength (ZeropageY _ _ _) = 2
binLength (Unknown _) = 1

nextAddr :: Instruction -> Word16
nextAddr inst = offset inst + binLength inst

followingAddr :: Instruction -> Maybe Word16
followingAddr (Absolute _ _ _) = Nothing
followingAddr inst = Just $ nextAddr inst

localBranch :: Instruction -> Maybe Word16
localBranch (Relative off _ arg) = Just $ fromIntegral $ offset + 2 + (fromIntegral arg)
  where offset :: Int = fromIntegral off
localBranch _ = Nothing

readInstruction :: Word16 -> AddressSpace -> Instruction
readInstruction offset mem = ri_ $ readStatic mem offset
  where ri_ 0x00 = readImplied BRK
        ri_ 0x09 = readImmediate ORA
        ri_ 0x10 = readRelative BPL
        ri_ 0x20 = readAbsolute JSR
        ri_ 0x4c = readAbsolute JMP
        ri_ 0x78 = readImplied PLA
        ri_ 0x8d = readAbsolute STA
        ri_ 0x9a = readImplied TXS
        ri_ 0xa0 = readImmediate LDY
        ri_ 0xa2 = readImmediate LDX
        ri_ 0xa9 = readImmediate LDA
        ri_ 0xad = readAbsolute LDA
        ri_ 0xb0 = readRelative BCS
        ri_ 0xbd = readAbsoluteX LDA
        ri_ 0xc9 = readImmediate CMP
        ri_ 0xca = readImplied DEX
        ri_ 0xd0 = readRelative BNE
        ri_ 0xd8 = readImplied CLD
        ri_ 0xee = readAbsolute INC
        ri_ x = error $ "Unknown code " ++ (showHex x "")
        readAbsolute opcode = Absolute offset opcode $ readAddress (offset + 1) mem
        readAbsoluteX opcode = AbsoluteX offset opcode $ readAddress (offset + 1) mem
        readImmediate opcode = Immediate offset opcode $ readStatic mem (offset + 1)
        readImplied opcode = Implied offset opcode
        readRelative opcode = Relative offset opcode $ (signed $ readStatic mem (offset + 1))

toAssembly :: Instruction -> String
toAssembly (Accumulator off op) = [fmt|{off:04x}: {op} A|]
toAssembly (Absolute off op arg) = [fmt|{off:04x}: {op} ${arg:04x}|]
toAssembly (AbsoluteX off op arg) = [fmt|{off:04x}: {op} ${arg:04x},X|]
toAssembly (AbsoluteY off op arg) = [fmt|{off:04x}: {op} ${arg:04x},Y|]
toAssembly (Immediate off op arg) = [fmt|{off:04x}: {op} #${arg:02x}|]
toAssembly (Implied off op) = [fmt|{off:04x}: {op}|]
toAssembly (Relative off op arg) = [fmt|{off:04x}: {op} #${arg:02} ({target:4x})|]
  where offset :: Int = fromIntegral off
        target = offset + 2 + (fromIntegral arg)

readInstructions :: Word16 -> AddressSpace -> [Instruction]
readInstructions offset mem = inst : readInstructions (nextAddr inst) mem
  where inst = readInstruction offset mem

functionBody :: Word16 -> AddressSpace -> [Instruction]
functionBody off mem = Map.elems $ functionBody_ mem off Map.empty

functionBody_ :: AddressSpace -> Word16 -> Map Word16 Instruction -> Map Word16 Instruction
functionBody_ mem off acc
  | Map.member off acc = acc
  | otherwise = foldr (functionBody_ mem) (Map.insert off inst acc) $ catMaybes [followingAddr inst, localBranch inst]
  where inst = readInstruction off mem