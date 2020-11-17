module AddressSpace (
  AddressSpace(..),
  irqAddress,
  readAddress,
  readRom,
  resetAddress
) where

import Data.Bits (shiftL)
import qualified Data.ByteString as BS
import Data.Word (Word16, Word8)

data AddressSpace = AddressSpace {
  readStatic :: Word16 -> Word8
}

readRom :: Word16 -> BS.ByteString -> Word16 -> Maybe Word8
readRom offset rom idx
  | idx < offset = Nothing
  | idx -  offset >= (fromIntegral $ BS.length rom) = Nothing
  | otherwise = Just $ BS.index rom (fromIntegral $ idx - offset)

readAddress :: Word16 -> AddressSpace -> Word16
readAddress offset mem = (fromIntegral $ readStatic mem offset) + (shiftL (fromIntegral $ readStatic mem (offset + 1)) 8)

resetAddress :: AddressSpace -> Word16
resetAddress = readAddress 0xfffc

irqAddress :: AddressSpace -> Word16
irqAddress = readAddress 0xfffe