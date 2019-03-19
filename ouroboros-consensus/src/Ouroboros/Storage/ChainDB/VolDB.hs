{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}

-- | Thin wrapper around the volatile DB
module Ouroboros.Storage.ChainDB.VolDB (
    VolDB -- Opaque
    -- * Initialization
  , VolDbArgs(..)
  , defaultArgs
  , openDB
    -- * Candidates
  , candidates
    -- * Getting and parsing blocks
  , getKnownHeader
  , getKnownBlock
  , getBlock
    -- * Wrappers
  , getIsMember
    -- * Re-exports
  , VolatileDBError
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Read as CBOR
import           Data.Bifunctor (second)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           System.FilePath ((</>))

import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.Block (ChainHash (..), HasHeader (..),
                     StandardHash)
import           Ouroboros.Network.ChainFragment (ChainFragment)
import qualified Ouroboros.Network.ChainFragment as Fragment

import qualified Ouroboros.Consensus.Util.CBOR as Util.CBOR

import           Ouroboros.Storage.ChainDB.API (ChainDbFailure (..))
import           Ouroboros.Storage.FS.API (HasFS)
import           Ouroboros.Storage.FS.API.Types (MountPoint (..))
import           Ouroboros.Storage.FS.IO (ioHasFS)
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH
import           Ouroboros.Storage.VolatileDB (VolatileDB, VolatileDBError)
import qualified Ouroboros.Storage.VolatileDB as VolDB

-- | Thin wrapper around the volatile DB (opaque type)
--
-- The intention is that all interaction with the volatile DB goes through
-- this module.
data VolDB m blk hdr = VolDB {
      volDB    :: VolatileDB (HeaderHash blk) m
    , decBlock :: forall s. Decoder s blk
    , header   :: blk -> hdr
    }

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

data VolDbArgs m blk hdr = forall h. VolDbArgs {
      volHasFS         :: HasFS m h
    , volErr           :: ErrorHandling (VolatileDBError (HeaderHash blk)) m
    , volBlocksPerFile :: Int
    , volDecodeBlock   :: forall s. Decoder s blk
    , volGetHeader     :: blk -> hdr
    }

-- | Default arguments when using the 'IO' monad
--
-- The following fields must still be defined:
--
-- * blocksPerFile
-- * decodeBlock
-- * getHeader
defaultArgs :: StandardHash blk => FilePath -> VolDbArgs IO blk hdr
defaultArgs fp = VolDbArgs {
      volErr   = EH.exceptions
    , volHasFS = ioHasFS $ MountPoint (fp </> "volatile")
      -- Fields without a default
    , volBlocksPerFile = error "no default for volBlocksPerFile"
    , volDecodeBlock   = error "no default for volDecodeBlock"
    , volGetHeader     = error "no default for volGetHeader"
    }

openDB :: (MonadCatch m, MonadSTM m, MonadST m, HasHeader blk)
       => VolDbArgs m blk hdr -> m (VolDB m blk hdr)
openDB args@VolDbArgs{..} = do
    volDB <- VolDB.openDB
               volHasFS
               volErr
               (blockFileParser args)
               volBlocksPerFile
    return $ VolDB volDB volDecodeBlock volGetHeader

{-------------------------------------------------------------------------------
  Wrappers
-------------------------------------------------------------------------------}

getIsMember :: VolDB m blk hdr -> STM m (HeaderHash blk -> Bool)
getIsMember db = withSTM db $ \vol -> shouldBeSTM (VolDB.getIsMember vol)
  where
    -- TODO: Wait for Kostas' PR
    shouldBeSTM :: m a -> STM m a
    shouldBeSTM = undefined

{-------------------------------------------------------------------------------
  Compute candidates
-------------------------------------------------------------------------------}

candidates :: forall m blk hdr. (MonadCatch m, HasHeader blk, HasHeader hdr)
           => VolDB m blk hdr -> ChainHash blk -> m [ChainFragment hdr]
candidates db start = do
    hashFragments <- withDB db $ \vol ->
                       hashes start <$> VolDB.getSuccessors vol
    mapM mkFragments hashFragments
  where
    -- current chain of headers i memory
    -- reading the few new blocks from disk is okay
    -- startup cost does not matters, Duncan is not worried.
    -- add ticket for tracking the size of our in-memory structures.

    -- 'hashes' constructs a list of hashes. We need to get ChainFragments of
    -- something that impleemnts HasHeader. Getting there currently requires
    -- reading blocks. We might need to reconsider this.
    mkFragments :: [HeaderHash blk] -> m (ChainFragment hdr)
    mkFragments = fmap (Fragment.fromOldestFirst) . mapM (getKnownHeader db)

    -- Construct fragments of hashes
    --
    -- We do this as a first step since this is pure function
    hashes :: ChainHash blk
           -> (Maybe (HeaderHash blk) -> Set (HeaderHash blk))
           -> [[HeaderHash blk]]
    hashes prev succsOf = do
        next <- Set.toList $ succsOf (fromChainHash prev)
        rest <- hashes (BlockHash next) succsOf
        return $ next : rest

{-------------------------------------------------------------------------------
  Getting and parsing blocks
-------------------------------------------------------------------------------}

getKnownHeader :: (MonadCatch m, StandardHash blk, Typeable blk)
               => VolDB m blk hdr -> HeaderHash blk -> m hdr
getKnownHeader db@VolDB{..} hash =
    header <$> getKnownBlock db hash

getKnownBlock :: (MonadCatch m, StandardHash blk, Typeable blk)
              => VolDB m blk hdr -> HeaderHash blk -> m blk
getKnownBlock db hash = do
    mBlock <- mustExist hash <$> getBlock db hash
    case mBlock of
      Right b  -> return b
      Left err -> throwM err

getBlock :: (MonadCatch m, StandardHash blk, Typeable blk)
         => VolDB m blk hdr -> HeaderHash blk -> m (Maybe blk)
getBlock db hash = do
    mBlob <- withDB db $ \vol ->
               fmap (parse (decBlock db) hash) <$> VolDB.getBlock vol hash
    case mBlob of
      Nothing         -> return $ Nothing
      Just (Right b)  -> return $ Just b
      Just (Left err) -> throwM $ err

{-------------------------------------------------------------------------------
  Auxiliary: parsing
-------------------------------------------------------------------------------}

blockFileParser :: forall m blk hdr. (MonadST m, MonadThrow m, HasHeader blk)
                => VolDbArgs m blk hdr
                -> VolDB.Parser
                     (VolDB.ParserError (HeaderHash blk))
                     m
                     (HeaderHash blk)
blockFileParser VolDbArgs{..} =
    VolDB.Parser $
        fmap (second (fmap mkErr))
      . Util.CBOR.readIncrementalOffsets volHasFS decoder'
  where
    decoder' :: forall s. Decoder s (VolDB.BlockInfo (HeaderHash blk))
    decoder' = extractInfo <$> volDecodeBlock

    extractInfo :: blk -> VolDB.BlockInfo (HeaderHash blk)
    extractInfo b = VolDB.BlockInfo {
          bbid    = blockHash b
        , bslot   = blockSlot b
        , bpreBid = fromChainHash (blockPrevHash b)
        }

    -- TODO: This won't be needed anymore after Kostas' next PR
    mkErr :: Util.CBOR.ReadIncrementalErr -> VolDB.ParserError (HeaderHash blk)
    mkErr = undefined

{-------------------------------------------------------------------------------
  Error handling
-------------------------------------------------------------------------------}

-- | Wrap calls to the volatile DB and rethrow exceptions that may indicate
-- disk failure and should therefore trigger recovery
withDB :: forall m blk hdr x. (MonadCatch m, StandardHash blk, Typeable blk)
       => VolDB m blk hdr
       -> (VolatileDB (HeaderHash blk) m -> m x)
       -> m x
withDB VolDB{..} k = catch (k volDB) rethrow
  where
    rethrow :: VolatileDBError (HeaderHash blk) -> m x
    rethrow err = case wrap err of
                    Just err' -> throwM err'
                    Nothing   -> throwM err

    -- We might have to revisit this
    -- See also https://github.com/input-output-hk/ouroboros-network/issues/428
    wrap :: VolatileDBError (HeaderHash blk) -> Maybe (ChainDbFailure blk)
    wrap (VolDB.FileSystemError err)   = Just (VolDbFileSystemError err)
    wrap VolDB.VParserError{}          = Nothing
    wrap VolDB.InvalidArgumentsError{} = Nothing
    wrap VolDB.ClosedDBError{}         = Nothing

-- | STM actions, by definition, cannot access the disk and therefore we don't
-- have to worry about catching exceptions here: any exceptions that may be
-- thrown indicate bugs, either in the ChainDB or in the volatile DB
withSTM :: VolDB m blk hdr
        -> (VolatileDB (HeaderHash blk) m -> STM m x)
        -> STM m x
withSTM VolDB{..} k = k volDB

mustExist :: HeaderHash blk
          -> Maybe blk
          -> Either (ChainDbFailure blk) blk
mustExist hash Nothing  = Left  $ VolDbMissingBlock hash
mustExist _    (Just b) = Right $ b

parse :: forall blk.
         (forall s. Decoder s blk)
      -> HeaderHash blk
      -> Strict.ByteString
      -> Either (ChainDbFailure blk) blk
parse dec hash =
    aux . CBOR.deserialiseFromBytes dec . Lazy.fromStrict
  where
    aux :: Either CBOR.DeserialiseFailure (Lazy.ByteString, blk)
        -> Either (ChainDbFailure blk) blk
    aux (Right (bs, b))
      | Lazy.null bs = Right b
      | otherwise    = Left $ VolDbTrailingData hash bs
    aux (Left err)   = Left $ VolDbParseFailure hash err

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

fromChainHash :: ChainHash blk -> Maybe (HeaderHash blk)
fromChainHash GenesisHash      = Nothing
fromChainHash (BlockHash hash) = Just hash
