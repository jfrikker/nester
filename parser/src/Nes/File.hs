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
  prgRom :: BS.ByteString,
  chrRom :: BS.ByteString
}

getNesFile :: Get NesFile
getNesFile = do
  skip 4
  prgSize <- getWord8
  chrSize <- getWord8
  skip 10
  prgRom <- getByteString $ fromIntegral prgSize * 16384
  chrRom <- getByteString $ fromIntegral chrSize * 8192
  return $ NesFile { prgRom = prgRom, chrRom = chrRom }