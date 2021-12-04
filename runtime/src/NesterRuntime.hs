{-# LANGUAGE QuasiQuotes #-}
module NesterRuntime (
  run
) where

import Control.Monad.State (get, liftIO, modify, put)
import qualified Control.Monad.State as State
import Data.Word(Word8, Word16)
import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT (($=))
import NesterRuntime.CPU (Callbacks(Callbacks, readCallback, writeCallback, advanceClock), RomM, nmi, reset)
import PyF (fmt)

advanceClock_ :: Word16 -> RomM Word16 ()
advanceClock_ clk = do
  modify (+ clk)

readCallback_ :: Word16 -> RomM Word16 Word8
readCallback_ addr = do
  clk <- get
  liftIO $ putStrLn [fmt|Reading: {addr:04x}    (cycles:{clk:04x})|]
  put 0
  return 0xf0

writeCallback_ :: Word16 -> Word8 -> RomM Word16 ()
writeCallback_ addr val = do
  clk <- get
  liftIO $ putStrLn [fmt|Writing: {addr:04x} {val:02x} (cycles:{clk:04})|]
  put 0

callbacks = Callbacks {
  advanceClock = advanceClock_,
  readCallback = readCallback_,
  writeCallback = writeCallback_
}

runIt = do
  reset callbacks
  nmi callbacks

run :: IO ()
run = do
  GLUT.getArgsAndInitialize
  GLUT.createWindow "Hello World"
  GLUT.windowSize $= GLUT.Size 320 200
  State.evalStateT (reset callbacks) 0