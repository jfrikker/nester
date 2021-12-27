{-# LANGUAGE QuasiQuotes #-}

module Main where

import Assembly (toAssembly)
import Data.Binary.Get (
  Decoder(..),
  pushChunk,
  pushEndOfInput,
  runGetIncremental
  )
import qualified Data.ByteString as BS
import Data.Foldable (for_)
import qualified Data.Map as Map
import Data.Function ((&))
import qualified Data.Text.Lazy.IO as TIO
import File (
  AppleFile(appleOffset, appleRom),
  NesFile(chrRom, prgRom),
  getNesFile, getAppleFile
  )
import LLVM (toIRNes)
import LLVM.Pretty (ppllvm)
import Mapper.Apple (appleMapper)
import Mapper.Base (
  Mapper,
  irqAddress,
  nmiAddress,
  resetAddress
  )
import Mapper.Mapper0 (mapper0)
import Options(
  Options(defineOptions),
  runSubcommand,
  simpleOption,
  subcommand
  )
import Passes (functionBodies, passBase, selfLoopPass, smbSwitchPass)
import PyF (fmt)
import System.IO (
  IOMode(WriteMode),
  hPutStrLn,
  withFile
  )

data EmptyOptions = EmptyOptions

instance Options EmptyOptions where
    defineOptions = pure EmptyOptions

data DisassembleOptions  = DisassembleOptions {
  disassembleFormat :: String
}

instance Options DisassembleOptions where
  defineOptions = pure DisassembleOptions
    <*> simpleOption "format" "nes"
        "The input file format"

disassemble :: EmptyOptions -> DisassembleOptions -> [String] -> IO ()
disassemble _ options [input, output] = do
  buf <- BS.readFile input
  let mem = readMapper (disassembleFormat options) buf
  withFile output WriteMode $ \h -> do
    let reset = resetAddress mem
    hPutStrLn h [fmt|; Reset: {reset:04x}|]
    let nmi = nmiAddress mem
    hPutStrLn h [fmt|; NMI: {nmi:04x}|]
    let irq = irqAddress mem
    hPutStrLn h [fmt|; IRQ: {irq:04x}|]
    hPutStrLn h ""
    let parser = selfLoopPass $ smbSwitchPass passBase
    let functions = functionBodies parser [reset, nmi, irq] mem
    for_ (Map.assocs functions) $ \(offset, body) -> do
      hPutStrLn h ""
      hPutStrLn h [fmt|{offset:04x}:|]
      for_ (Map.elems body) $ hPutStrLn h . toAssembly

data LlvmOptions  = LlvmOptions {
  llvmFormat :: String
}

instance Options LlvmOptions where
  defineOptions = pure LlvmOptions
    <*> simpleOption "format" "nes"
        "The input file format"

llvm :: EmptyOptions -> LlvmOptions -> [String] -> IO ()
llvm _ options [input, output] = do
  buf <- BS.readFile input
  let mem = readMapper (llvmFormat options) buf
  withFile output WriteMode $ \h -> do
    let reset = resetAddress mem
    let nmi = nmiAddress mem
    let irq = irqAddress mem
    let parser = selfLoopPass $ smbSwitchPass passBase
    let functions = functionBodies parser [reset, nmi, irq] mem
    TIO.hPutStrLn h $ ppllvm $ toIRNes functions mem

readMapper :: String -> BS.ByteString -> Mapper
readMapper "nes" buf = let Done _ _ file = runGetIncremental getNesFile `pushChunk` buf & pushEndOfInput
  in mapper0 (prgRom file) $ error "No chr rom"
readMapper "apple" buf = let Done _ _ file = runGetIncremental getAppleFile `pushChunk` buf & pushEndOfInput
  in appleMapper (appleOffset file) (appleRom file)

main :: IO ()
main = do
  runSubcommand [subcommand "disassemble" disassemble, subcommand "llvm" llvm]
