{-# LANGUAGE ForeignFunctionInterface #-}

module NesterRuntime.CPU (
  RomM,
  reset
) where

import Control.Monad.State (StateT, liftIO)
import qualified Control.Monad.State as State
import Data.IORef (newIORef, writeIORef, readIORef, IORef)
import Data.Word(Word8, Word16)
import Foreign (
  FunPtr,
  nullFunPtr
  )

foreign import ccall "reset" romReset :: FunPtr (Word16 -> IO Word8) -> FunPtr (Word16 -> Word8 -> IO ()) -> FunPtr (() -> IO ()) -> IO ()
foreign import ccall "wrapper" mkReadCb :: (Word16 -> IO Word8) -> IO (FunPtr (Word16 -> IO Word8))
foreign import ccall "wrapper" mkWriteCb :: (Word16 -> Word8 -> IO ()) -> IO (FunPtr (Word16 -> Word8 -> IO ()))

type RomM s a = StateT s IO a

realReadCallback :: IORef s -> (Word16 -> RomM s Word8) -> Word16 -> IO Word8
realReadCallback ref cb addr = do
  current <- readIORef ref
  (res, newState) <- State.runStateT (cb addr) current
  writeIORef ref newState
  return res

realWriteCallback :: IORef s -> (Word16 -> Word8 -> RomM s ()) -> Word16 -> Word8 -> IO ()
realWriteCallback ref cb addr val = do
  current <- readIORef ref
  (res, newState) <- State.runStateT (cb addr val) current
  writeIORef ref newState
  return res

reset :: (Word16 -> RomM s Word8) -> (Word16 -> Word8 -> RomM s ()) -> RomM s ()
reset read write = do
  current <- State.get
  ref <- liftIO $ newIORef current
  rc <- liftIO $ mkReadCb $ realReadCallback ref read
  wc <- liftIO $ mkWriteCb $ realWriteCallback ref write
  let sc = nullFunPtr
  liftIO $ romReset rc wc sc