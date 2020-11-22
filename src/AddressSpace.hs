module AddressSpace (
  AddressSpace(..),
  irqAddress,
  nmiAddress,
  readAddress,
  readRom,
  readValue,
  resetAddress
) where

import Data.Bits (shiftL)
import qualified Data.ByteString as BS
import Data.Functor ((<&>))
import Data.Word (Word16, Word8)

data AddressSpace = AddressSpace {
  readStatic :: Word16 -> Word8
}

readRom :: Word16 -> BS.ByteString -> Word16 -> Maybe Word8
readRom offset rom idx
  | idx < offset = Nothing
  | idx -  offset >= (fromIntegral $ BS.length rom) = Nothing
  | otherwise = Just $ BS.index rom (fromIntegral $ idx - offset)

readValue :: Word16 -> AddressSpace -> Word8
readValue = flip readStatic

readAddress :: Word16 -> AddressSpace -> Word16
readAddress offset = do
  low <- readValue offset <&> fromIntegral
  high <- readValue (offset + 1) <&> fromIntegral
  return $ low + (shiftL high 8)

resetAddress :: AddressSpace -> Word16
resetAddress = readAddress 0xfffc

irqAddress :: AddressSpace -> Word16
irqAddress = readAddress 0xfffe

nmiAddress :: AddressSpace -> Word16
nmiAddress = readAddress 0xfffa