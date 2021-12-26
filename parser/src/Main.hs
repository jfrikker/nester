{-# LANGUAGE QuasiQuotes #-}

module Main where

import Mapper (
  Mapper,
  irqAddress,
  mapper0,
  appleMapper,
  nmiAddress,
  readRom,
  resetAddress
  )
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
import Data.Maybe (fromJust)
import Data.Function ((&))
import qualified Data.Text.Lazy.IO as TIO
import LLVM (toIRNes)
import LLVM.Pretty (ppllvm)
import File (
  AppleFile(appleOffset, appleRom),
  NesFile(chrRom, prgRom),
  getNesFile, getAppleFile
  )
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
  let mem = readFile buf
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
  where readFile = readFileInner $ disassembleFormat options
        readFileInner "nes" buf = let Done _ _ file = runGetIncremental getNesFile `pushChunk` buf & pushEndOfInput
          in mapper0 (prgRom file) $ error "No chr rom"
        readFileInner "apple" buf = let Done _ _ file = runGetIncremental getAppleFile `pushChunk` buf & pushEndOfInput
          in appleMapper (appleOffset file) (appleRom file)

llvm :: EmptyOptions -> EmptyOptions -> [String] -> IO ()
llvm _ _ [input, output] = do
  buf <- BS.readFile input
  let Done _ _ file = runGetIncremental getNesFile `pushChunk` buf & pushEndOfInput
  let mem = mapper0 (prgRom file) $ chrRom file
  withFile output WriteMode $ \h -> do
    let reset = resetAddress mem
    let nmi = nmiAddress mem
    let irq = irqAddress mem
    let parser = selfLoopPass $ smbSwitchPass passBase
    let functions = functionBodies parser [reset, nmi, irq] mem
    TIO.hPutStrLn h $ ppllvm $ toIRNes functions mem

main :: IO ()
main = do
  runSubcommand [subcommand "disassemble" disassemble, subcommand "llvm" llvm]
