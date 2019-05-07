{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}

module Ouroboros.Storage.ChainDB.Impl (
    -- * Initializatio
    ChainDbArgs(..)
  , defaultArgs
  , openDB
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Data.Set (Set)
import qualified Data.Set as Set

import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as Fragment
import           Ouroboros.Network.Block (ChainHash (..), HasHeader (..), Point,
                     StandardHash)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Chain (genesisPoint)

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Storage.ChainDB.API
import           Ouroboros.Storage.Common
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling,
                     ThrowCantCatch)

import           Ouroboros.Storage.ChainDB.ImmDB (ImmDB)
import qualified Ouroboros.Storage.ChainDB.ImmDB as ImmDB
import           Ouroboros.Storage.ChainDB.LgrDB (LgrDB)
import qualified Ouroboros.Storage.ChainDB.LgrDB as LgrDB
import           Ouroboros.Storage.ChainDB.VolDB (VolDB)
import qualified Ouroboros.Storage.ChainDB.VolDB as VolDB

{-------------------------------------------------------------------------------
  Arguments
-------------------------------------------------------------------------------}

data ChainDbArgs m blk hdr = forall h1 h2 h3. ChainDbArgs {

      -- Decoders

      cdbDecodeHash       :: forall s. Decoder s (HeaderHash blk)
    , cdbDecodeBlock      :: forall s. Decoder s blk
    , cdbDecodeLedger     :: forall s. Decoder s (LedgerState blk)
    , cdbDecodeChainState :: forall s. Decoder s (ChainState (BlockProtocol blk))

      -- Encoders

    , cdbEncodeHash       :: HeaderHash blk -> Encoding
    , cdbEncodePreHeader  :: PreHeader blk -> Encoding

      -- Error handling

    , cdbErrImmDb         :: ErrorHandling ImmDB.ImmutableDBError m
    , cdbErrVolDb         :: ErrorHandling (VolDB.VolatileDBError (HeaderHash blk)) m
    , cdbErrVolDbSTM      :: ThrowCantCatch (VolDB.VolatileDBError (HeaderHash blk)) (STM m)

      -- HasFS instances

    , cdbHasFSImmDb       :: HasFS m h1
    , cdbHasFSVolDb       :: HasFS m h2
    , cdbHasFSLgrDB       :: HasFS m h3

      -- Policy

    , cdbValidation       :: ImmDB.ValidationPolicy
    , cdbBlocksPerFile    :: Int
    , cdbMemPolicy        :: LgrDB.MemPolicy

      -- Integration

    , cdbNodeConfig       :: NodeConfig (BlockProtocol blk)
    , cdbEpochSize        :: EpochNo -> m EpochSize
    , cdbIsEBB            :: blk -> Maybe (HeaderHash blk)
    , cdbGetHeader        :: blk -> hdr
    , cdbGenesis          :: m (ExtLedgerState blk)
    }

-- | Default arguments for use within IO
--
-- See 'ImmDB.defaultArgs' and 'VolDB.defaultArgs' for a list of which fields
-- are not given a default and must therefore be set explicitly.
defaultArgs :: StandardHash blk => FilePath -> ChainDbArgs IO blk hdr
defaultArgs fp = toChainDbArgs ( ImmDB.defaultArgs fp
                               , VolDB.defaultArgs fp
                               , LgrDB.defaultArgs fp
                               )

-- | Internal: split chain DB args into imm DB and vol DB args
fromChainDbArgs :: ChainDbArgs m blk hdr
                -> ( ImmDB.ImmDbArgs m blk
                   , VolDB.VolDbArgs m blk hdr
                   , LgrDB.LgrDbArgs m blk
                   )
fromChainDbArgs ChainDbArgs{..} = (
      ImmDB.ImmDbArgs {
          immDecodeHash       = cdbDecodeHash
        , immDecodeBlock      = cdbDecodeBlock
        , immEncodeHash       = cdbEncodeHash
        , immErr              = cdbErrImmDb
        , immEpochSize        = cdbEpochSize
        , immValidation       = cdbValidation
        , immIsEBB            = cdbIsEBB
        , immHasFS            = cdbHasFSImmDb
        }
    , VolDB.VolDbArgs {
          volHasFS            = cdbHasFSVolDb
        , volErr              = cdbErrVolDb
        , volErrSTM           = cdbErrVolDbSTM
        , volBlocksPerFile    = cdbBlocksPerFile
        , volDecodeBlock      = cdbDecodeBlock
        , volGetHeader        = cdbGetHeader
        }
    , LgrDB.LgrDbArgs {
          lgrNodeConfig       = cdbNodeConfig
        , lgrHasFS            = cdbHasFSLgrDB
        , lgrDecodeLedger     = cdbDecodeLedger
        , lgrDecodeChainState = cdbDecodeChainState
        , lgrDecodeHash       = cdbDecodeHash
        , lgrEncodePreHeader  = cdbEncodePreHeader
        , lgrMemPolicy        = cdbMemPolicy
        , lgrGenesis          = cdbGenesis
        }
    )

-- | Internal: construct chain DB args from imm DB and vol DB
--
-- Useful in 'defaultArgs'
toChainDbArgs :: ( ImmDB.ImmDbArgs m blk
                 , VolDB.VolDbArgs m blk hdr
                 , LgrDB.LgrDbArgs m blk
                 )
              -> ChainDbArgs m blk hdr
toChainDbArgs ( ImmDB.ImmDbArgs{..}
              , VolDB.VolDbArgs{..}
              , LgrDB.LgrDbArgs{..}
              ) = ChainDbArgs{
      -- Decoders
      cdbDecodeHash       = immDecodeHash
    , cdbDecodeBlock      = immDecodeBlock
    , cdbDecodeLedger     = lgrDecodeLedger
    , cdbDecodeChainState = lgrDecodeChainState
      -- Encoders
    , cdbEncodeHash       = immEncodeHash
    , cdbEncodePreHeader  = lgrEncodePreHeader
      -- Error handling
    , cdbErrImmDb         = immErr
    , cdbErrVolDb         = volErr
    , cdbErrVolDbSTM      = volErrSTM
      -- HasFS instances
    , cdbHasFSImmDb       = immHasFS
    , cdbHasFSVolDb       = volHasFS
    , cdbHasFSLgrDB       = lgrHasFS
      -- Policy
    , cdbValidation       = immValidation
    , cdbBlocksPerFile    = volBlocksPerFile
    , cdbMemPolicy        = lgrMemPolicy
      -- Integration
    , cdbNodeConfig       = lgrNodeConfig
    , cdbEpochSize        = immEpochSize
    , cdbIsEBB            = immIsEBB
    , cdbGetHeader        = volGetHeader
    , cdbGenesis          = lgrGenesis
    }

{-------------------------------------------------------------------------------
  Internal environment
-------------------------------------------------------------------------------}

data ChainDbEnv m blk hdr = CDB {
      cdbImmDB   :: ImmDB m blk
    , cdbVolDB   :: VolDB m blk hdr
    , cdbLgrDB   :: LgrDB m blk
    , cdbChain   :: TVar m (AnchoredFragment hdr)
    , cdbInvalid :: TVar m (Set (Point blk))
    , cdbHeader  :: blk -> hdr
    }

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

openDB :: forall m blk hdr.
          ( MonadSTM   m
          , MonadST    m
          , MonadCatch m
          , HasHeader hdr
          , HeaderHash hdr ~ HeaderHash blk
          , ProtocolLedgerView blk
          )
       => ChainDbArgs m blk hdr -> m (ChainDB m blk hdr)
openDB args = do
    immDB   <- ImmDB.openDB argsImmDb
    volDB   <- VolDB.openDB argsVolDb
    lgrDB   <- LgrDB.openDB argsLgrDb
                            immDB
                            (implGetKnownBlock immDB volDB)
    chain   <- atomically $ newTVar undefined
    invalid <- atomically $ newTVar Set.empty
    let env = CDB { cdbImmDB   = immDB
                  , cdbVolDB   = volDB
                  , cdbLgrDB   = lgrDB
                  , cdbChain   = chain
                  , cdbInvalid = invalid
                  , cdbHeader  = cdbGetHeader args
                  }
    return ChainDB {
        addBlock           = undefined
      , getCurrentChain    = cdbGetCurrentChain    env
      , getCurrentLedger   = undefined
      , getTipBlock        = cdbGetTipBlock        env
      , getTipHeader       = cdbGetTipHeader       env
      , getTipPoint        = cdbGetTipPoint        env
      , getBlock           = cdbGetBlock           env
      , getIsFetched       = cdbGetIsFetched       env
      , streamBlocks       = cdbStreamBlocks       env
      , readBlocks         = undefined
      , readHeaders        = undefined
      , knownInvalidBlocks = cdbKnownInvalidBlocks env
      , pointOnChain       = undefined
      }
  where
    (argsImmDb, argsVolDb, argsLgrDb) = fromChainDbArgs args

{-------------------------------------------------------------------------------
  Implementation
-------------------------------------------------------------------------------}

cdbGetIsFetched :: forall m blk hdr. MonadSTM m
                => ChainDbEnv m blk hdr
                -> STM m (Point blk -> Bool)
cdbGetIsFetched CDB{..} = basedOnHash <$> VolDB.getIsMember cdbVolDB
  where
    -- The volatile DB indexes by hash only, not by points. However, it should
    -- not be possible to have two points with the same hash but different
    -- slot numbers.
    basedOnHash :: (HeaderHash blk -> Bool) -> Point blk -> Bool
    basedOnHash f p =
        case Block.pointHash p of
          BlockHash hash -> f hash
          GenesisHash    -> False

cdbGetCurrentChain :: MonadSTM m
                   => ChainDbEnv m blk hdr
                   -> STM m (AnchoredFragment hdr)
cdbGetCurrentChain CDB{..} = readTVar cdbChain

cdbGetTipPoint :: ( MonadSTM m
                  , HasHeader hdr
                  , HeaderHash hdr ~ HeaderHash blk
                  )
               => ChainDbEnv m blk hdr
               -> STM m (Point blk)
cdbGetTipPoint = fmap (Block.castPoint . Fragment.headPoint)
               . cdbGetCurrentChain

cdbGetTipBlock :: ( MonadCatch m
                  , MonadSTM m
                  , HasHeader blk
                  , HasHeader hdr
                  , HeaderHash hdr ~ HeaderHash blk
                  )
               => ChainDbEnv m blk hdr
               -> m (Maybe blk)
cdbGetTipBlock cdb@CDB{..} = do
    tipPoint <- atomically $ cdbGetTipPoint cdb
    if tipPoint == genesisPoint
      then return Nothing
      else Just <$> implGetKnownBlock cdbImmDB cdbVolDB tipPoint

cdbGetTipHeader :: ( MonadSTM m
                   , HasHeader hdr
                   )
                => ChainDbEnv m blk hdr
                -> m (Maybe hdr)
cdbGetTipHeader CDB{..} =
    eitherToMaybe . Fragment.head <$> atomically (readTVar cdbChain)
  where
    eitherToMaybe = either (const Nothing) Just

cdbKnownInvalidBlocks :: MonadSTM m
                      => ChainDbEnv m blk hdr
                      -> STM m (Set (Point blk))
cdbKnownInvalidBlocks CDB{..} = readTVar cdbInvalid

cdbGetBlock :: ( MonadCatch m
               , MonadSTM m
               , HasHeader blk
               )
            => ChainDbEnv m blk hdr
            -> Point blk -> m (Maybe blk)
cdbGetBlock CDB{..} = implGetBlock cdbImmDB cdbVolDB

cdbStreamBlocks :: ChainDbEnv m blk hdr
                -> StreamFrom blk -> StreamTo blk -> m (Iterator m blk)
cdbStreamBlocks CDB{..} = implStreamBlocks cdbImmDB cdbVolDB

{-------------------------------------------------------------------------------
  Lower level functionality

  These are functions that don't require all parts of the ChainDB to have
  been initialized
-------------------------------------------------------------------------------}

implGetBlock :: ( MonadCatch m
                , MonadSTM m
                , HasHeader blk
                )
             => ImmDB m blk
             -> VolDB m blk hdr
             -> Point blk
             -> m (Maybe blk)
implGetBlock immDB volDB point = case Block.pointHash point of
    GenesisHash    -> return Nothing
    BlockHash hash -> do
      isInVolDB <- ($ hash) <$> atomically (VolDB.getIsMember volDB)
      -- TODO is this the best way to determine whether the volatile or the
      -- immutable DB should store a block?
      if isInVolDB
        then Just <$> VolDB.getKnownBlock volDB hash
        -- TODO what if the point refers to an EBB? When the hash of the block
        -- doesn't match that of the point, we know it, but then we need to
        -- come up with the right EpochNo for the EBB. (Ideally, we just read
        -- one block instead of two).
        else ImmDB.getBlock immDB (Right (Block.pointSlot point))

implStreamBlocks :: ImmDB m blk
                 -> VolDB m blk hdr
                 -> StreamFrom blk -> StreamTo blk -> m (Iterator m blk)
implStreamBlocks = undefined

-- | Wrapper around 'implGetBlock' for blocks that we know should exist
--
-- If the block does /not/ exist, this is an indication of disk failure and
-- should trigger recovery.
--
-- PRECONDITION: the point may not refer to genesis
implGetKnownBlock :: ( MonadCatch m
                     , MonadSTM m
                     , HasHeader blk
                     )
                  => ImmDB m blk
                  -> VolDB m blk hdr
                  -> Point blk
                  -> m blk
implGetKnownBlock immDB volDB point = case Block.pointHash point of
    GenesisHash    -> error "implGetKnownBlock: point may not refer to genesis"
    BlockHash hash -> do
      isInVolDB <- ($ hash) <$> atomically (VolDB.getIsMember volDB)
      -- TODO see comment in 'implGetBlock'
      if isInVolDB
        then VolDB.getKnownBlock volDB hash
        -- TODO see comment in 'implGetBlock'
        else ImmDB.getKnownBlock immDB (Right (Block.pointSlot point))
