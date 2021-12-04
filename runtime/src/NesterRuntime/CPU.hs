{-# LANGUAGE ForeignFunctionInterface #-}

module NesterRuntime.CPU (
  Callbacks(..),
  RomM,
  nmi,
  reset
) where

import Control.Monad.State (StateT, liftIO)
import qualified Control.Monad.State as State
import Data.IORef (newIORef, writeIORef, readIORef, IORef)
import Data.Word (Word8, Word16)
import Foreign (FunPtr)

foreign import ccall "reset" romReset :: FunPtr (Word8 -> Word16 -> Word8 -> IO Word8) -> IO ()
foreign import ccall "nmi" romNmi :: FunPtr (Word8 -> Word16 -> Word8 -> IO Word8) -> IO ()
foreign import ccall "wrapper" mkCb :: (Word8 -> Word16 -> Word8 -> IO Word8) -> IO (FunPtr (Word8 -> Word16 -> Word8 -> IO Word8))

type RomM s a = StateT s IO a

data Callbacks s  = Callbacks {
  advanceClock :: Word16 -> RomM s (),
  readCallback :: Word16 -> RomM s Word8,
  writeCallback :: Word16 -> Word8 -> RomM s ()
}

realCallback :: IORef s -> Callbacks s -> Word8 -> Word16 -> Word8 -> IO Word8
realCallback ref cbs t addr val = do
  current <- readIORef ref
  (res, newState) <- State.runStateT action current
  writeIORef ref newState
  return res
  where action = case t of
                   0 -> readCallback cbs addr
                   1 -> writeCallback cbs addr val >> return 0
                   2 -> advanceClock cbs addr >> return 0

reset :: Callbacks s -> RomM s ()
reset cbs = do
  current <- State.get
  ref <- liftIO $ newIORef current
  cb <- liftIO $ mkCb $ realCallback ref cbs
  liftIO $ romReset cb

nmi :: Callbacks s -> RomM s ()
nmi cbs = do
  current <- State.get
  ref <- liftIO $ newIORef current
  ref <- liftIO $ newIORef current
  cb <- liftIO $ mkCb $ realCallback ref cbs
  liftIO $ romNmi cb