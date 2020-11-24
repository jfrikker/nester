{-# LANGUAGE DataKinds, QuasiQuotes, ScopedTypeVariables, TypeFamilies #-}

module Assembly (
  Instruction(..),
  Opcode(..),
  functionBody,
  functionBodyWithParser,
  functionBodies,
  functionBodiesWithParser,
  nextAddr,
  followingAddrs,
  offset,
  readAddress,
  readInstruction,
  readInstructions,
  toAssembly
) where

import AddressSpace (AddressSpace, readAddress, readValue)
import Control.Monad.Extra (iterateM)
import Data.Int(Int8)
import Data.Foldable (foldrM)
import Data.Functor.Identity (Identity(runIdentity))
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word(Word8, Word16)
import Data.Maybe (mapMaybe)
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
  TYA |
  SWA
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
  Switch Word16 Opcode Word16 [Word16] |
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
offset (Switch off _ _ _) = off
offset (Unknown off _) = off

binLength :: Instruction -> Word16
binLength Accumulator {} = 1
binLength Absolute {} = 3
binLength AbsoluteX {} = 3
binLength AbsoluteY {} = 3
binLength Immediate {} = 2
binLength Implied {} = 1
binLength Indirect {} = 3
binLength IndirectX {} = 2
binLength IndirectY {} = 2
binLength Relative {} = 2
binLength Zeropage {} = 2
binLength ZeropageX {} = 2
binLength ZeropageY {} = 2
binLength (Switch _ _ len _) = len
binLength Unknown {} = 1

nextAddr :: Instruction -> Word16
nextAddr inst = offset inst + binLength inst

followingAddrs :: Instruction -> [Word16]
followingAddrs (Absolute _ JMP addr) = [addr]
followingAddrs (Implied _ RTS) = []
followingAddrs (Indirect _ JMP _) = []
followingAddrs inst@(Relative off _ arg) = [nextAddr inst, fromIntegral $ offset + 2 + fromIntegral arg]
  where offset :: Int = fromIntegral off
followingAddrs (Switch _ _ _ addrs) = addrs
followingAddrs (Unknown _ _) = []
followingAddrs inst = [nextAddr inst]

callTarget :: Instruction -> Maybe Word16
callTarget (Absolute _ JSR arg) = Just arg
callTarget _ = Nothing

readInstruction :: Word16 -> AddressSpace -> Instruction
readInstruction offset = readValue offset >>= ri_
  where ri_ 0x00 = readImplied BRK
        ri_ 0x01 = readIndirectX ORA
        ri_ 0x05 = readZeropage ORA
        ri_ 0x06 = readZeropage ASL
        ri_ 0x08 = readImplied PHP
        ri_ 0x09 = readImmediate ORA
        ri_ 0x0a = readAccumulator ASL
        ri_ 0x0d = readAbsolute ORA
        ri_ 0x0e = readAbsolute ASL
        ri_ 0x10 = readRelative BPL
        ri_ 0x11 = readIndirectY ORA
        ri_ 0x15 = readZeropageX ORA
        ri_ 0x16 = readZeropageX ASL
        ri_ 0x18 = readImplied CLC
        ri_ 0x19 = readAbsoluteY ORA
        ri_ 0x1d = readAbsoluteX ORA
        ri_ 0x1e = readAbsoluteX ASL
        ri_ 0x20 = readAbsolute JSR
        ri_ 0x21 = readIndirectX AND
        ri_ 0x24 = readZeropage BIT
        ri_ 0x25 = readZeropage AND
        ri_ 0x26 = readZeropage ROL
        ri_ 0x28 = readImplied PLP
        ri_ 0x29 = readImmediate AND
        ri_ 0x2a = readAccumulator ROL
        ri_ 0x2c = readAbsolute BIT
        ri_ 0x2d = readAbsolute AND
        ri_ 0x2e = readAbsolute ROL
        ri_ 0x30 = readRelative BMI
        ri_ 0x31 = readIndirectY AND
        ri_ 0x35 = readZeropageX AND
        ri_ 0x36 = readZeropageX ROL
        ri_ 0x38 = readImplied SEC
        ri_ 0x39 = readAbsoluteY AND
        ri_ 0x3d = readAbsoluteX AND
        ri_ 0x3e = readAbsoluteX ROL
        ri_ 0x40 = readImplied RTI
        ri_ 0x41 = readIndirectX EOR
        ri_ 0x45 = readZeropage EOR
        ri_ 0x46 = readZeropage LSR
        ri_ 0x48 = readImplied PHA
        ri_ 0x49 = readImmediate EOR
        ri_ 0x4a = readAccumulator LSR
        ri_ 0x4c = readAbsolute JMP
        ri_ 0x4d = readAbsolute EOR
        ri_ 0x4e = readAbsolute LSR
        ri_ 0x50 = readRelative BVC
        ri_ 0x51 = readIndirectY EOR
        ri_ 0x55 = readZeropageX EOR
        ri_ 0x56 = readZeropageX LSR
        ri_ 0x58 = readImplied CLI
        ri_ 0x59 = readAbsoluteY EOR
        ri_ 0x5d = readAbsoluteX EOR
        ri_ 0x5e = readAbsoluteX LSR
        ri_ 0x60 = readImplied RTS
        ri_ 0x61 = readIndirectX ADC
        ri_ 0x65 = readZeropage ADC
        ri_ 0x66 = readZeropage ROR
        ri_ 0x68 = readImplied PLA
        ri_ 0x69 = readImmediate ADC
        ri_ 0x6a = readAccumulator ROR
        ri_ 0x6c = readIndirect JMP
        ri_ 0x6d = readAbsolute ADC
        ri_ 0x6e = readAbsolute ROR
        ri_ 0x70 = readRelative BVS
        ri_ 0x71 = readIndirectY ADC
        ri_ 0x75 = readZeropageX ADC
        ri_ 0x76 = readZeropageX ROR
        ri_ 0x78 = readImplied SEI
        ri_ 0x79 = readAbsoluteY ADC
        ri_ 0x7d = readAbsoluteX ADC
        ri_ 0x7e = readAbsoluteX ROR
        ri_ 0x81 = readIndirectX STA
        ri_ 0x84 = readZeropage STY
        ri_ 0x85 = readZeropage STA
        ri_ 0x86 = readZeropage STX
        ri_ 0x88 = readImplied DEY
        ri_ 0x8a = readImplied TXA
        ri_ 0x8c = readAbsolute STY
        ri_ 0x8d = readAbsolute STA
        ri_ 0x8e = readAbsolute STX
        ri_ 0x90 = readRelative BCC
        ri_ 0x91 = readIndirectY STA
        ri_ 0x94 = readZeropageX STY
        ri_ 0x95 = readZeropageX STA
        ri_ 0x96 = readZeropageY STX
        ri_ 0x98 = readImplied TAY
        ri_ 0x99 = readAbsoluteY STA
        ri_ 0x9a = readImplied TXS
        ri_ 0x9d = readAbsoluteX STA
        ri_ 0xa0 = readImmediate LDY
        ri_ 0xa1 = readIndirectX LDA
        ri_ 0xa2 = readImmediate LDX
        ri_ 0xa4 = readZeropage LDY
        ri_ 0xa5 = readZeropage LDA
        ri_ 0xa6 = readZeropage LDX
        ri_ 0xa8 = readImplied TAY
        ri_ 0xa9 = readImmediate LDA
        ri_ 0xaa = readImplied TAX
        ri_ 0xac = readAbsolute LDY
        ri_ 0xad = readAbsolute LDA
        ri_ 0xae = readAbsolute LDX
        ri_ 0xb0 = readRelative BCS
        ri_ 0xb1 = readIndirectY LDA
        ri_ 0xb4 = readZeropageX LDY
        ri_ 0xb5 = readZeropageX LDA
        ri_ 0xb6 = readZeropageY LDX
        ri_ 0xb8 = readImplied CLV
        ri_ 0xb9 = readAbsoluteY LDA
        ri_ 0xba = readImplied TSX
        ri_ 0xbc = readAbsoluteX LDY
        ri_ 0xbd = readAbsoluteX LDA
        ri_ 0xbe = readAbsoluteY LDX
        ri_ 0xc0 = readImmediate CPY
        ri_ 0xc1 = readIndirectX CMP
        ri_ 0xc4 = readZeropage CPY
        ri_ 0xc5 = readZeropage CMP
        ri_ 0xc6 = readZeropage DEC
        ri_ 0xc8 = readImplied INY
        ri_ 0xc9 = readImmediate CMP
        ri_ 0xca = readImplied DEX
        ri_ 0xcc = readAbsolute CPY
        ri_ 0xcd = readAbsolute CMP
        ri_ 0xce = readAbsolute DEC
        ri_ 0xd0 = readRelative BNE
        ri_ 0xd1 = readIndirectY CMP
        ri_ 0xd5 = readZeropageX CMP
        ri_ 0xd6 = readZeropageX DEC
        ri_ 0xd8 = readImplied CLD
        ri_ 0xd9 = readAbsoluteY CMP
        ri_ 0xdd = readAbsoluteX CMP
        ri_ 0xde = readAbsoluteX DEC
        ri_ 0xe0 = readImmediate CPX
        ri_ 0xe1 = readIndirectY SBC
        ri_ 0xe4 = readZeropage CPX
        ri_ 0xe5 = readZeropage SBC
        ri_ 0xe6 = readZeropage INC
        ri_ 0xe8 = readImplied INX
        ri_ 0xe9 = readImmediate SBC
        ri_ 0xea = readImplied NOP
        ri_ 0xec = readAbsolute CPX
        ri_ 0xed = readAbsolute SBC
        ri_ 0xee = readAbsolute INC
        ri_ 0xf0 = readRelative BEQ
        ri_ 0xf1 = readIndirectY SBC
        ri_ 0xf5 = readZeropageX SBC
        ri_ 0xf6 = readZeropageX INC
        ri_ 0xf8 = readImplied SED
        ri_ 0xf9 = readAbsoluteY SBC
        ri_ 0xfd = readAbsoluteX SBC
        ri_ 0xfe = readAbsoluteX INC
        ri_ x = return $ Unknown offset x
        readAbsolute opcode = Absolute offset opcode <$> readAddress nextOffset
        readAbsoluteX opcode = AbsoluteX offset opcode <$> readAddress nextOffset
        readAbsoluteY opcode = AbsoluteY offset opcode <$> readAddress nextOffset
        readAccumulator opcode = return $ Accumulator offset opcode
        readImmediate opcode = Immediate offset opcode <$> readValue nextOffset
        readImplied opcode = return $ Implied offset opcode
        readIndirect opcode = Indirect offset opcode <$> readAddress nextOffset
        readIndirectX opcode = IndirectX offset opcode <$> readValue nextOffset
        readIndirectY opcode = IndirectY offset opcode <$> readValue nextOffset
        readRelative opcode = Relative offset opcode . fromIntegral <$> readValue nextOffset
        readZeropage opcode = Zeropage offset opcode <$> readValue nextOffset
        readZeropageX opcode = ZeropageX offset opcode <$> readValue nextOffset
        readZeropageY opcode = ZeropageY offset opcode <$> readValue nextOffset
        nextOffset = offset + 1

toAssembly :: Instruction -> String
toAssembly (Accumulator off op) = [fmt|{off:04x}: {op} A|]
toAssembly (Absolute off op arg) = [fmt|{off:04x}: {op} ${arg:04x}|]
toAssembly (AbsoluteX off op arg) = [fmt|{off:04x}: {op} ${arg:04x},X|]
toAssembly (AbsoluteY off op arg) = [fmt|{off:04x}: {op} ${arg:04x},Y|]
toAssembly (Immediate off op arg) = [fmt|{off:04x}: {op} #${arg:02x}|]
toAssembly (Implied off op) = [fmt|{off:04x}: {op}|]
toAssembly (Indirect off op arg) = [fmt|{off:04x}: {op} (${arg:04x})|]
toAssembly (IndirectX off op arg) = [fmt|{off:04x}: {op} (${arg:02x},X)|]
toAssembly (IndirectY off op arg) = [fmt|{off:04x}: {op} (${arg:02x}),Y|]
toAssembly (Relative off op arg) = [fmt|{off:04x}: {op} ({target:04x})|]
  where offset :: Int = fromIntegral off
        target = offset + 2 + fromIntegral arg
toAssembly (Unknown off opcode) = [fmt|{off:04x}: !Unknown {opcode:02x}|]
toAssembly (Zeropage off op arg) = [fmt|{off:04x}: {op} ${arg:02x}|]
toAssembly (ZeropageX off op arg) = [fmt|{off:04x}: {op} ${arg:02x},X|]
toAssembly (ZeropageY off op arg) = [fmt|{off:04x}: {op} ${arg:02x},Y|]
toAssembly (Switch off op _ addrs) = [fmt|{off:04x}: {op} [{showAddrs}]|]
  where showAddrs = intercalate ", " $ map (\a -> [fmt|{a:04x}|]) addrs

readInstructions :: Word16 -> AddressSpace -> [Instruction]
readInstructions off = do
  inst <- readInstruction off
  rest <- iterateM (readInstruction . nextAddr) inst
  return (inst : rest)

callTargets :: [Instruction] -> [Word16]
callTargets = mapMaybe callTarget

functionBodies :: [Word16] -> AddressSpace -> Map Word16 (Map Word16 Instruction)
functionBodies = functionBodiesWithParserM readInstruction

functionBodiesWithParser :: (Word16 -> Instruction) -> [Word16] -> Map Word16 (Map Word16 Instruction)
functionBodiesWithParser p = runIdentity . functionBodiesWithParserM (return . p)

functionBodiesWithParserM :: Monad m => (Word16 -> m Instruction) -> [Word16] -> m (Map Word16 (Map Word16 Instruction))
functionBodiesWithParserM p = reachableM functionBodiesWithParser_
  where functionBodiesWithParser_ off = do
          body <- functionBodyWithParserM p off
          return (body, callTargets $ Map.elems body)

functionBody :: Word16 -> AddressSpace -> Map Word16 Instruction
functionBody = functionBodyWithParserM readInstruction

functionBodyWithParser :: (Word16 -> Instruction) -> Word16 -> Map Word16 Instruction
functionBodyWithParser p = runIdentity . functionBodyWithParserM (return . p)

functionBodyWithParserM :: Monad m => (Word16 -> m Instruction) -> Word16 -> m (Map Word16 Instruction)
functionBodyWithParserM p off = reachableM functionBodyWithParserM_ [off]
  where functionBodyWithParserM_ off = do
          inst <- p off
          return (inst, followingAddrs inst)

reachableM :: (Ord a, Monad m) => (a -> m (b, [a])) -> [a] -> m (Map a b)
reachableM f = foldrM reachableM_ Map.empty
  where reachableM_ key acc
          | Map.member key acc = return acc
          | otherwise = do
            (val, next) <- f key
            let nextAcc = Map.insert key val acc
            foldrM reachableM_ nextAcc next