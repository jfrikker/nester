{-# LANGUAGE ForeignFunctionInterface #-}

module NesterRuntime.CPU (

) where

import Control.Monad.State (StateT, liftIO)
import qualified Control.Monad.State as State
import Data.IORef (newIORef, writeIORef, readIORef, IORef)
import Data.Word(Word8, Word16)
import Foreign (
  FunPtr,
  Ptr,
  nullFunPtr,
  nullPtr
  )
import Foreign.Storable (Storable(alignment, poke, sizeOf))

data Callbacks = Callbacks {
  readCallback :: FunPtr (Word16 -> IO Word8),
  writeCallback :: FunPtr (Word16 -> Word8 -> IO ()),
  sleepCallback :: FunPtr (() -> IO ())
}

instance Storable Callbacks where
  sizeOf cb = sizeOf (readCallback cb) + sizeOf (writeCallback cb) + sizeOf (sleepCallback cb)
  alignment = 8
  poke ptr cb = 
  peek ptr = do
    a <- peekByteOff ptr 0
    b <- peekByteOff ptr 4
    return (MyStructType a b)

foreign import ccall "reset" romReset :: Ptr Callbacks -> IO ()
foreign import ccall "wrapper" mkReadCb :: (Word16 -> IO Word8) -> IO (FunPtr (Word16 -> IO Word8))

type RomM s a = StateT s IO a

realReadCallback :: IORef s -> (Word16 -> RomM s Word8) -> Word16 -> IO Word8
realReadCallback ref cb addr = do
  current <- readIORef ref
  (res, newState) <- State.runStateT (cb addr) current
  writeIORef ref newState
  return res

reset :: (Word16 -> RomM s Word8) -> (Word16 -> Word8 -> RomM s ()) -> RomM s () -> RomM s ()
reset read write sleep = do
  current <- State.get
  ref <- liftIO $ newIORef current
  rc <- liftIO $ mkReadCb $ realReadCallback ref read
  let wc = nullFunPtr
  let sc = nullFunPtr
  let callbacks = Callbacks {
    readCallback = rc,
    writeCallback = wc,
    sleepCallback = sc
  }
  let callbacksPtr = nullPtr
  liftIO $ poke callbacksPtr callbacks
  liftIO $ romReset callbacksPtr