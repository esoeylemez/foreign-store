{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Store a stable pointer in a foreign context to be retrieved
-- later. Persists through GHCi reloads.

module Foreign.Store
  (newStore
  ,lookupStore
  ,readStore
  ,deleteStore
  ,Store(..))
  where

import Control.Exception
import Data.Typeable
import Data.Word
import Foreign.Ptr
import Foreign.StablePtr

data StoreException
  = StoreNotFound
  deriving (Show,Eq,Typeable)

instance Exception StoreException

-- | A hideously unsafe store. Only for use if you are suave.
data Store a =
  Store Word32
  deriving (Show,Eq)

-- | Lookup from the store.
lookupStore :: Word32 -> IO (Maybe (Store a))
lookupStore i =
  do r <- x_lookup i
     if r == 0
        then return Nothing
        else return (Just (Store i))

-- | Make a new store. The internal vector of stores grows in
-- side. When stores are deleted the vector does not shrink, but old
-- slots are re-used.
newStore :: a -> IO (Store a)
newStore a =
  do sptr <- newStablePtr a
     i <- x_store sptr
     return (Store i)

-- | Read from the store. If the store has been deleted, this will
-- throw an exception.
readStore :: Store a -> IO a
readStore (Store i) =
  do sptr <- x_get i
     if castStablePtrToPtr sptr == nullPtr
        then throw StoreNotFound
        else deRefStablePtr sptr

-- | Frees the stable pointer for GC and frees up the slot in the
-- store. Deleting an already deleted store is a no-op.
deleteStore :: Store a -> IO ()
deleteStore (Store i) = do
  sptr <- x_get i
  if castStablePtrToPtr sptr == nullPtr
     then return ()
     else do freeStablePtr sptr
             x_delete i

foreign import ccall
  "x-helpers.h x_store"
  x_store :: StablePtr a -> IO Word32

foreign import ccall
  "x-helpers.h x_get"
  x_get :: Word32 -> IO (StablePtr a)

foreign import ccall
  "x-helpers.h x_lookup"
  x_lookup :: Word32 -> IO Word32

foreign import ccall
  "x-helpers.h x_delete"
  x_delete :: Word32 -> IO ()
