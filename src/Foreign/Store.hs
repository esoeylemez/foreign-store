{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Store a stable pointer in a foreign context to be retrieved
-- later. Persists through GHCi reloads. Not thread-safe.

module Foreign.Store
  (-- * Foreign stores
   writeStore
  ,newStore
  ,lookupStore
  ,readStore
  ,deleteStore
  ,storeAction
  ,withStore
  ,Store(..)
  ,StoreException(..))
  where

import Control.Exception
import Data.Typeable
import Data.Word
import Foreign.Ptr
import Foreign.StablePtr

-- | An exception when working with stores.
data StoreException
  = StoreNotFound
  deriving (Show,Eq,Typeable)

instance Exception StoreException

-- | A hideously unsafe store. Only for use if you are suave.
newtype Store a =
  Store Word32
  deriving (Show,Eq)

-- | Lookup from the store if an index is allocated.
lookupStore :: Word32 -> IO (Maybe (Store a))
lookupStore i =
  do r <- x_lookup i
     if r == 0
        then return Nothing
        else return (Just (Store i))

-- | Allocates or finds an unallocated store. The index is random. The
-- internal vector of stores grows in size. When stores are deleted
-- the vector does not shrink, but old slots are re-used.
-- Not thread-safe.
newStore :: a -> IO (Store a)
newStore a =
  do sptr <- newStablePtr a
     i <- x_store sptr
     return (Store i)

-- | Write to the store at the given index. If a store doesn't exist,
-- creates one and resizes the store vector to fit. If there is
-- already a store at the given index, deletes that store with
-- 'deleteStore' before replacing it.
-- Not thread-safe.
writeStore :: Store a -> a -> IO ()
writeStore (Store i) a =
  do existing <- lookupStore i
     maybe (return ()) deleteStore existing
     sptr <- newStablePtr a
     x_set i sptr
     return ()

-- | Read from the store. If the store has been deleted or is
-- unallocated, this will throw an exception.
-- Not thread-safe.
readStore :: Store a -> IO a
readStore (Store i) =
  do sptr <- x_get i
     if castStablePtrToPtr sptr == nullPtr
        then throw StoreNotFound
        else deRefStablePtr sptr

-- | Frees the stable pointer for GC and frees up the slot in the
-- store. Deleting an already deleted store is a no-op.
-- Not thread-safe.
deleteStore :: Store a -> IO ()
deleteStore (Store i) = do
  sptr <- x_get i
  if castStablePtrToPtr sptr == nullPtr
     then return ()
     else do freeStablePtr sptr
             x_delete i

-- | Run the action and store the result.
storeAction :: Store a -> IO a -> IO a
storeAction s m =
  do v <- m
     writeStore s v
     return v

-- | Run the action and store the result.
withStore :: Store a -> (a -> IO b) -> IO b
withStore s f =
  do v <- readStore s
     f v

foreign import ccall
  "x-helpers.h x_store"
  x_store :: StablePtr a -> IO Word32

foreign import ccall
  "x-helpers.h x_set"
  x_set :: Word32 -> StablePtr a -> IO ()

foreign import ccall
  "x-helpers.h x_get"
  x_get :: Word32 -> IO (StablePtr a)

foreign import ccall
  "x-helpers.h x_lookup"
  x_lookup :: Word32 -> IO Word32

foreign import ccall
  "x-helpers.h x_delete"
  x_delete :: Word32 -> IO ()
