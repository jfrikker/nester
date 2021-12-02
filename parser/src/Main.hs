{-# LANGUAGE QuasiQuotes #-}

module Main where

import AddressSpace (
  AddressSpace(AddressSpace, readStatic),
  irqAddress,
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
import Nes.File (
  NesFile(chrRom, prgRom),
  getNesFile)
import Options(
  Options(defineOptions),
  runSubcommand,
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

disassemble :: EmptyOptions -> EmptyOptions -> [String] -> IO ()
disassemble _ _ [input, output] = do
  buf <- BS.readFile input
  let Done _ _ file = runGetIncremental getNesFile `pushChunk` buf & pushEndOfInput
  let mem = mapper0 $ prgRom file
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

llvm :: EmptyOptions -> EmptyOptions -> [String] -> IO ()
llvm _ _ [input, output] = do
  buf <- BS.readFile input
  let Done _ _ file = runGetIncremental getNesFile `pushChunk` buf & pushEndOfInput
  let mem = mapper0 $ prgRom file
  withFile output WriteMode $ \h -> do
    let reset = resetAddress mem
    let nmi = nmiAddress mem
    let irq = irqAddress mem
    let parser = selfLoopPass $ smbSwitchPass passBase
    let functions = functionBodies parser [reset, nmi, irq] mem
    TIO.hPutStrLn h $ ppllvm $ toIRNes functions (prgRom file) (chrRom file) mem

mapper0 :: BS.ByteString -> AddressSpace
mapper0 rom = AddressSpace { readStatic = readStatic }
  where readStatic = fromJust . readRom 32768 rom

main :: IO ()
main = do
  runSubcommand [subcommand "disassemble" disassemble, subcommand "llvm" llvm]
