module NesterRuntime.PPU (

) where
import Data.Array.IO (IOUArray)
import qualified Data.Array.MArray as MArray
import Data.Word (Word8, Word16)
import Foreign (Ptr)

foreign import ccall "getMapperId" getMapperId :: IO Word8
foreign import ccall "getChrRom" getChrRom :: IO (Ptr Word8)

data PPU = PPU {
  ppuctrl :: Word8,
  rom :: Ptr Word8,
  ram :: IOUArray Word16 Word8
}

ppu :: IO PPU
ppu = do
  rom <- getChrRom
  ram <- MArray.newArray_ (0, 0x4000)
  return PPU {
    ppuctrl = 0,
    rom = rom,
    ram = ram
  }

writePpu :: Word16 -> Word8 -> PPU -> PPU
writePpu 0x2000 val ppu = ppu { ppuctrl = val}
writePpu _ _ ppu = ppu