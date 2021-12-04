{-# LANGUAGE ForeignFunctionInterface #-}

module NesterRuntime.CPU (
  RomM,
  nmi,
  reset
) where

import Control.Monad.State (StateT, liftIO)
import qualified Control.Monad.State as State
import Data.IORef (newIORef, writeIORef, readIORef, IORef)
import Data.Word (Word8, Word16)
import Foreign (FunPtr)

foreign import ccall "reset" romReset :: FunPtr (Word16 -> Word16 -> IO Word8) -> FunPtr (Word16 -> Word8 -> Word16 -> IO ()) -> IO ()
foreign import ccall "nmi" romNmi :: FunPtr (Word16 -> Word16 -> IO Word8) -> FunPtr (Word16 -> Word8 -> Word16 -> IO ()) -> IO ()
foreign import ccall "wrapper" mkReadCb :: (Word16 -> Word16 -> IO Word8) -> IO (FunPtr (Word16 -> Word16 -> IO Word8))
foreign import ccall "wrapper" mkWriteCb :: (Word16 -> Word8 -> Word16 -> IO ()) -> IO (FunPtr (Word16 -> Word8 -> Word16 -> IO ()))

type RomM s a = StateT s IO a

realReadCallback :: IORef s -> (Word16 -> Word16 -> RomM s Word8) -> Word16 -> Word16 -> IO Word8
realReadCallback ref cb addr clk = do
  current <- readIORef ref
  (res, newState) <- State.runStateT (cb addr clk) current
  writeIORef ref newState
  return res

realWriteCallback :: IORef s -> (Word16 -> Word8 -> Word16 -> RomM s ()) -> Word16 -> Word8 -> Word16 -> IO ()
realWriteCallback ref cb addr val clk = do
  current <- readIORef ref
  (res, newState) <- State.runStateT (cb addr val clk) current
  writeIORef ref newState
  return res

reset :: (Word16 -> Word16 -> RomM s Word8) -> (Word16 -> Word8 -> Word16 -> RomM s ()) -> RomM s ()
reset read write = do
  current <- State.get
  ref <- liftIO $ newIORef current
  rc <- liftIO $ mkReadCb $ realReadCallback ref read
  wc <- liftIO $ mkWriteCb $ realWriteCallback ref write
  liftIO $ romReset rc wc

nmi :: (Word16 -> Word16 -> RomM s Word8) -> (Word16 -> Word8 -> Word16 -> RomM s ()) -> RomM s ()
nmi read write = do
  current <- State.get
  ref <- liftIO $ newIORef current
  rc <- liftIO $ mkReadCb $ realReadCallback ref read
  wc <- liftIO $ mkWriteCb $ realWriteCallback ref write
  liftIO $ romNmi rc wc