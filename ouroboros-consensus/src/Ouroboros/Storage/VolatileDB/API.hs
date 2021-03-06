{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Ouroboros.Storage.VolatileDB.API
  ( VolatileDB(..)
  , withDB

  , module Ouroboros.Storage.VolatileDB.Types
  ) where

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (Builder)
import           Data.Set (Set)
import           GHC.Stack (HasCallStack)

import           Ouroboros.Storage.VolatileDB.Types

-- | Open the database using the given function, perform the given action
-- using the database, and closes the database using its 'closeDB' function,
-- in case of success or when an exception was raised.
withDB :: (HasCallStack, MonadThrow m)
       => m (VolatileDB blockId m)
          -- ^ How to open the database
       -> (VolatileDB blockId m -> m a)
          -- ^ Action to perform using the database
       -> m a
withDB openDB = bracket openDB closeDB

data VolatileDB blockId m = VolatileDB {
      closeDB        :: HasCallStack => m ()
    , isOpenDB       :: HasCallStack => m Bool
    , reOpenDB       :: HasCallStack => m ()
    , getBlock       :: HasCallStack => blockId -> m (Maybe ByteString)
    , putBlock       :: HasCallStack => BlockInfo blockId -> Builder -> m ()
    , getBlockIds    :: HasCallStack => m [blockId]
    , getSuccessors  :: HasCallStack => m (Maybe blockId -> Set blockId)
    , garbageCollect :: HasCallStack => SlotNo -> m ()
    , getIsMember    :: HasCallStack => STM m (blockId -> Bool)
}
