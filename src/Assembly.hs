{-# LANGUAGE DataKinds, QuasiQuotes, ScopedTypeVariables, TypeFamilies #-}

module Assembly (
  Instruction(..),
  Opcode(..),
  functionBody,
  functionBodies,
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
  Unknown Word16 Word8
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
offset (Unknown off _) = off

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
binLength (Unknown _ _) = 1

nextAddr :: Instruction -> Word16
nextAddr inst = offset inst + binLength inst

followingAddr :: Instruction -> Maybe Word16
followingAddr (Absolute _ JMP addr) = Just addr
followingAddr (Implied _ RTS) = Nothing
followingAddr (Unknown _ _) = Nothing
followingAddr inst = Just $ nextAddr inst

localBranch :: Instruction -> Maybe Word16
localBranch (Relative off _ arg) = Just $ fromIntegral $ offset + 2 + (fromIntegral arg)
  where offset :: Int = fromIntegral off
localBranch _ = Nothing

callTarget :: Instruction -> Maybe Word16
callTarget (Absolute _ JSR arg) = Just arg
callTarget _ = Nothing

readInstruction :: Word16 -> AddressSpace -> Instruction
readInstruction offset mem = ri_ $ readStatic mem offset
  where ri_ 0x00 = readImplied BRK
        ri_ 0x09 = readImmediate ORA
        ri_ 0x10 = readRelative BPL
        ri_ 0x20 = readAbsolute JSR
        ri_ 0x29 = readImmediate AND
        ri_ 0x2c = readAbsolute BIT
        ri_ 0x4c = readAbsolute JMP
        ri_ 0x60 = readImplied RTS
        ri_ 0x6d = readAbsolute ADC
        ri_ 0x78 = readImplied PLA
        ri_ 0x85 = readZeropage STA
        ri_ 0x86 = readZeropage STX
        ri_ 0x88 = readImplied DEY
        ri_ 0x8a = readImplied TXA
        ri_ 0x8d = readAbsolute STA
        ri_ 0x91 = readIndirectY STA
        ri_ 0x99 = readAbsoluteY STA
        ri_ 0x9a = readImplied TXS
        ri_ 0xa0 = readImmediate LDY
        ri_ 0xa2 = readImmediate LDX
        ri_ 0xa9 = readImmediate LDA
        ri_ 0xad = readAbsolute LDA
        ri_ 0xb0 = readRelative BCS
        ri_ 0xbd = readAbsoluteX LDA
        ri_ 0xc0 = readImmediate CPY
        ri_ 0xc9 = readImmediate CMP
        ri_ 0xc8 = readImplied INY
        ri_ 0xca = readImplied DEX
        ri_ 0xd0 = readRelative BNE
        ri_ 0xd8 = readImplied CLD
        ri_ 0xe0 = readImmediate CPX
        ri_ 0xee = readAbsolute INC
        ri_ 0xf0 = readRelative BEQ
        ri_ x = Unknown offset x
        readAbsolute opcode = Absolute offset opcode $ readAddress (offset + 1) mem
        readAbsoluteX opcode = AbsoluteX offset opcode $ readAddress (offset + 1) mem
        readAbsoluteY opcode = AbsoluteY offset opcode $ readAddress (offset + 1) mem
        readImmediate opcode = Immediate offset opcode $ readStatic mem (offset + 1)
        readImplied opcode = Implied offset opcode
        readIndirectY opcode = IndirectY offset opcode $ readStatic mem (offset + 1)
        readRelative opcode = Relative offset opcode $ (signed $ readStatic mem (offset + 1))
        readZeropage opcode = Zeropage offset opcode $ readStatic mem (offset + 1)

toAssembly :: Instruction -> String
toAssembly (Accumulator off op) = [fmt|{off:04x}: {op} A|]
toAssembly (Absolute off op arg) = [fmt|{off:04x}: {op} ${arg:04x}|]
toAssembly (AbsoluteX off op arg) = [fmt|{off:04x}: {op} ${arg:04x},X|]
toAssembly (AbsoluteY off op arg) = [fmt|{off:04x}: {op} ${arg:04x},Y|]
toAssembly (Immediate off op arg) = [fmt|{off:04x}: {op} #${arg:02x}|]
toAssembly (Implied off op) = [fmt|{off:04x}: {op}|]
toAssembly (IndirectY off op arg) = [fmt|{off:04x}: {op} (${arg:02x}),Y|]
toAssembly (Relative off op arg) = [fmt|{off:04x}: {op} #${arg:02} ({target:04x})|]
  where offset :: Int = fromIntegral off
        target = offset + 2 + (fromIntegral arg)
toAssembly (Unknown off opcode) = [fmt|{off:04x}: !Unknown {opcode:02x}|]
toAssembly (Zeropage off op arg) = [fmt|{off:04x}: {op} ${arg:02x}|]
toAssembly i = show i

readInstructions :: Word16 -> AddressSpace -> [Instruction]
readInstructions offset mem = inst : readInstructions (nextAddr inst) mem
  where inst = readInstruction offset mem

callTargets :: [Instruction] -> [Word16]
callTargets = catMaybes . map callTarget

functionBodies :: Word16 -> AddressSpace -> Map Word16 [Instruction]
functionBodies offset mem = functionBodies_ offset Map.empty
  where functionBodies_ offset acc
          | Map.member offset acc = acc
          | otherwise = foldr functionBodies_ (Map.insert offset body acc) $ callTargets body
          where body = functionBody offset mem

functionBody :: Word16 -> AddressSpace -> [Instruction]
functionBody off mem = Map.elems $ functionBody_ mem off Map.empty

functionBody_ :: AddressSpace -> Word16 -> Map Word16 Instruction -> Map Word16 Instruction
functionBody_ mem off acc
  | Map.member off acc = acc
  | otherwise = foldr (functionBody_ mem) (Map.insert off inst acc) $ catMaybes [followingAddr inst, localBranch inst]
  where inst = readInstruction off mem