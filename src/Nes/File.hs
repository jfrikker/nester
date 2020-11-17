module Nes.File (
  NesFile(..),
  getNesFile
) where

import Data.Binary.Get (
  Get,
  getByteString,
  getWord8,
  skip
  )
import qualified Data.ByteString as BS

data NesFile = NesFile {
  prgRom :: BS.ByteString
}

getNesFile :: Get NesFile
getNesFile = do
  skip 4
  size <- getWord8
  skip 11
  prgRom <- getByteString $ (fromIntegral size) * 16384
  return $ NesFile { prgRom = prgRom }