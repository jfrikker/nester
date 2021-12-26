module File (
  NesFile(..),
  getNesFile,
  AppleFile(..),
  getAppleFile,
) where

import Data.Binary (Word16)
import Data.Binary.Get (
  Get,
  getByteString,
  getWord8,
  getWord16be,
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

data AppleFile = AppleFile {
  appleOffset :: Word16,
  appleRom :: BS.ByteString
}

getAppleFile :: Get AppleFile
getAppleFile = do
  offset <- getWord16be
  len <- getWord16be
  rom <- getByteString  $ fromIntegral len
  return $ AppleFile { appleOffset = offset, appleRom = rom }