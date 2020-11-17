module Main where

import AddressSpace (
  AddressSpace(AddressSpace, readStatic),
  readAddress,
  readRom,
  resetAddress
  )
import Assembly (
  functionBody,
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
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Function ((&))
import qualified Data.Text.Lazy.IO as TIO
import LLVM (toIRNes)
import LLVM.Pretty (ppllvm)
import Nes.File (
  NesFile(prgRom),
  getNesFile)
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
  -- let instructions = take 40 $ readInstructions (resetAddress addressSpace) addressSpace
  let instructions = Map.elems $ functionBody (resetAddress addressSpace) addressSpace
  for_ instructions $ putStrLn . toAssembly
  TIO.putStrLn $ ppllvm $ toIRNes addressSpace
  return ()
