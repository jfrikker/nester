{-# LANGUAGE QuasiQuotes #-}
module NesterRuntime (
  run
) where

import Control.Monad.State (liftIO)
import qualified Control.Monad.State as State
import Data.Word(Word8, Word16)
import NesterRuntime.CPU (RomM, reset)
import PyF (fmt)

readCallback :: Word16 -> RomM () Word8
readCallback addr = do
  liftIO $ putStrLn [fmt|Reading: {addr:04x}|]
  return 0xf0

writeCallback :: Word16 -> Word8 -> RomM () ()
writeCallback addr val = do
  liftIO $ putStrLn [fmt|Writing: {addr:04x} {val:02x}|]

run :: IO ()
run = State.evalStateT (reset readCallback writeCallback) ()