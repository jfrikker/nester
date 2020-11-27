{-# LANGUAGE QuasiQuotes #-}

module Main where

import AddressSpace (
  AddressSpace(AddressSpace, readStatic),
  irqAddress,
  nmiAddress,
  readAddress,
  readRom,
  resetAddress
  )
import Assembly (
  readInstructions,
  toAssembly
  )
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
  NesFile(prgRom),
  getNesFile)
import Passes (functionBodies, passBase, smbSwitchPass)
import PyF (fmt)
import System.Environment (getArgs)

mapper0 :: BS.ByteString -> AddressSpace
mapper0 rom = AddressSpace { readStatic = readStatic }
  where readStatic = fromJust . readRom 32768 rom

main :: IO ()
main = do
  args <- getArgs
  let file = head args
  buf <- BS.readFile file
  let Done _ _ file = runGetIncremental getNesFile `pushChunk` buf & pushEndOfInput
  let mem = mapper0 $ prgRom file
  -- print $ resetAddress mem
  -- print $ irqAddress mem
  -- print $ nmiAddress mem
  -- let instructions = take 40 $ readInstructions (resetAddress addressSpace) addressSpace
  -- let instructions = functionBody 0x90cc addressSpace
  let parser = smbSwitchPass passBase
  let functions = functionBodies parser [resetAddress mem, irqAddress mem, nmiAddress mem] mem
  -- for_ (Map.assocs functions) $ \(offset, body) -> do
  --   putStrLn ""
  --   putStrLn [fmt|{offset:04x}:|]
  --   for_ (Map.elems body) $ putStrLn . toAssembly
  TIO.putStrLn $ ppllvm $ toIRNes functions mem
  return ()
