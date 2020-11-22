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
  functionBodies,
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
  let addressSpace = mapper0 $ prgRom file
  print $ resetAddress addressSpace
  print $ irqAddress addressSpace
  print $ nmiAddress addressSpace
  -- let instructions = take 40 $ readInstructions (resetAddress addressSpace) addressSpace
  -- let instructions = functionBody 0x90cc addressSpace
  let functions = functionBodies [resetAddress addressSpace, irqAddress addressSpace, nmiAddress addressSpace] addressSpace
  for_ functions $ \(offset, body) -> do
    putStrLn ""
    putStrLn [fmt|{offset:04x}:|]
    for_ body $ putStrLn . toAssembly
  -- TIO.putStrLn $ ppllvm $ toIRNes addressSpace
  return ()
