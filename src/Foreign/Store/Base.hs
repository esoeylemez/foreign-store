-- | Higher level API than Foreign.Store
module Foreign.Store.Base where

import Foreign.Store
import Data.Word
import Control.Concurrent
import Data.IORef

takeStoredMVar :: Word32 -> IO (MVar a)
takeStoredMVar storeNum = do
    var <- readStore (Store storeNum)
    _ <- takeMVar var 
    return var 

modifyStoredIORef :: Store (IORef a) -> (a -> IO a) -> IO ()
modifyStoredIORef store f = do
  ref <- readStore store
  readIORef ref >>= f >>= writeIORef ref 

newStoredIORef :: a -> IO (IORef a)
newStoredIORef x = do
  ref <- newIORef x
  _ <- newStore ref 
  return ref 

newStoredMVar :: IO (MVar a)
newStoredMVar = do
  var <- newEmptyMVar
  _ <- newStore var 
  return var
